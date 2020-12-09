// SPDX-License-Identifier: Apache-2.0

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


abstract class NCOIdentityNodeBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: NCOParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val nco = Module(new NCOStreamingPINCandPOFF(params))

    // Control registers
    val poff   = RegInit(0.U((beatBytes*8).W))
    val poff_valid = RegInit(true.B)
    val out_ready = RegInit(true.B)

    // Define register fields
    val fields = Seq(
      // settable registers
      RegField(beatBytes*8, poff,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
      RegField(1,           poff_valid, RegFieldDesc(name = "poff valid",  desc = "phase offset valid")),
      RegField(1,           out_ready, RegFieldDesc(name = "out ready",  desc = "output ready"))
    )

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

    out.bits.data := nco.io.out.bits.real.asTypeOf(UInt(32.W))
    out.valid     := nco.io.out.valid
    out.ready     := DontCare
    nco.io.freq.valid     := in.valid
    nco.io.freq.bits   := in.bits.data.asTypeOf(params.protoFreq)
    nco.io.poff.valid    := poff_valid
    nco.io.poff.bits   := poff.asTypeOf(params.protoFreq)
    nco.io.out.ready     := out_ready
  }
}

class AXI4NCOIdentityNodeBlock[T <: Data : Real: BinaryRepresentation](params: NCOParams[T], address: AddressSet, _beatBytes: Int = 4)(implicit p: Parameters) extends NCOIdentityNodeBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes)) // use AXI4 memory mapped
}

object NCOIdentityNodeDspBlock extends App
{
  val paramsNCO = FixedNCOParams(
    tableSize = 128,
    tableWidth = 16,
    phaseWidth = 9,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = true,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Streaming
  )

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val NCOIdentityNodeModule = LazyModule(new AXI4NCOIdentityNodeBlock(paramsNCO, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with dspblocks.AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })

  chisel3.Driver.execute(args, ()=> NCOIdentityNodeModule.module) // generate verilog code
}
