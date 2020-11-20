package nco

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

import dsptools.tester.MemMasterModel
import freechips.rocketchip.amba.axi4
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

abstract class NCOBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: NCOParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(
    Seq(AXI4StreamMasterParameters(
      "ncoOut",
      n = beatBytes
  )))))

  lazy val module = new LazyModuleImp(this) {
  val (out, _) = streamNode.out(0)

  //  NCO module
  val nco = Module(new NCONoStreaming(params))

  // Control registers
  val freq   = RegInit(1.U((beatBytes*4).W))
  val poff   = RegInit(0.U((beatBytes*4).W))
  val freq_valid = RegInit(true.B)
  val poff_valid = RegInit(true.B)
  val out_ready = RegInit(true.B)

  // Define register fields
  val fields = Seq(
    // settable registers
    RegField(beatBytes*4, freq,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
    RegField(1,           freq_valid, RegFieldDesc(name = "freq valid",  desc = "frequency valid")),
    RegField(beatBytes*4, poff,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
    RegField(1,           poff_valid, RegFieldDesc(name = "poff valid",  desc = "phase offset valid")),
    RegField(1,           out_ready, RegFieldDesc(name = "out ready",  desc = "output ready"))
  )

  // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
  regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

  out.bits.data := nco.io.out.bits.real.asTypeOf(UInt(32.W))
  out.valid     := nco.io.out.valid
  out.ready     := DontCare
  if (params.pincType == Config){
    nco.io.freq.get   := freq.asTypeOf(params.protoFreq)
  }
  if (params.poffType == Config){
    nco.io.poff.get   := poff.asTypeOf(params.protoFreq)
  }
  nco.io.out.ready     := out_ready
  }
}


trait AXI4Block extends DspBlock[
  AXI4MasterPortParameters,
  AXI4SlavePortParameters,
  AXI4EdgeParameters,
  AXI4EdgeParameters,
  AXI4Bundle] {
    def standaloneParams = AXI4BundleParameters(addrBits = 64, dataBits = 64, idBits = 1)
    val ioMem = mem.map { m => {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }}

    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

    ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode

    val out = InModuleBody { ioOutNode.makeIO() }

}


class AXI4NCOBlock[T <: Data : Real: BinaryRepresentation](params: NCOParams[T], address: AddressSet, _beatBytes: Int = 8)(implicit p: Parameters) extends NCOBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes)) // use AXI4 memory mapped
}


abstract class NCOBlockTester[T <: Data : Real: BinaryRepresentation, D, U, EO, EI, B <: Data](c: NCOBlock[T, D, U, EO, EI, B]) extends PeekPokeTester(c.module) with MemMasterModel {
  memWriteWord(0, 2)
  step(100)
  memWriteWord(0, 4)
  step(200)
}

// specialize the generic tester for axi4
class AXI4NCOBlockTester[T <: Data : Real: BinaryRepresentation](c: AXI4NCOBlock[T] with AXI4Block) extends NCOBlockTester(c) with AXI4MasterModel {
  def memAXI = c.ioMem.get
}


object AXI4NCODspBlock extends App
{

  // here just define parameters
  val paramsNCO = FixedNCOParams(
    tableSize = 128,
    tableWidth = 16,
    phaseWidth = 9,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Config,
    poffType = Config
  )

  implicit val p: Parameters = Parameters.empty

  val ncoModule = LazyModule(new AXI4NCOBlock(paramsNCO, AddressSet(0x000000, 0xFF), _beatBytes = 8) with AXI4Block {
    override def standaloneParams = AXI4BundleParameters(addrBits = 64, dataBits = 64, idBits = 1)
  })
  chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), () => ncoModule.module) { _ => new AXI4NCOBlockTester(ncoModule) }
}


