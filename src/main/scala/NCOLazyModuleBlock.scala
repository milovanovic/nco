package nco

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import dsptools.tester.MemMasterModel
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import org.scalatest.{FlatSpec, Matchers}



abstract class NCOLazyModuleBlock[T <: Data : Real : BinaryRepresentation](params: NCOParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with AXI4DspBlock  {

  def regmap(mapping: RegField.Map*)
  def addCSR(address: Int, field: Seq[RegField]): Unit = {}
  
  val freq = if (params.pincType == Streaming) Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters())) else None
  val poff = if (params.poffType == Streaming) Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters())) else None

  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))

  lazy val module = new LazyModuleImp(this) {
    val ioout = streamNode.out(0)._1
    
    val phaseConverter = Module(new NCOTable(params.tableParams))
    val phaseCounter   = RegInit(UInt(params.phaseWidth.W), 0.U)

    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // pinc & poff streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    if (params.pincType == Streaming && params.poffType == Streaming) {
      
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      
      if (params.useMultiplier) {
        val fields = Seq(
        RegField(1, enableMultiplying,
          RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
        RegField(params.protoOut.getWidth, multiplyingFactor,
          RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      
      val latency = 2 // if (params.syncROMEnable) 2 else 1
      
      // phase counter
      if (params.phaseAccEnable) {
        when(freq.get.in(0)._1.fire()) {
          phaseCounter := phaseCounter + freq.get.in(0)._1.bits.data.asUInt()
        }
        when (poff.get.in(0)._1.fire()) {
          phaseConverter.io.phase := phaseCounter + poff.get.in(0)._1.bits.data.asUInt()
        }.otherwise {
          phaseConverter.io.phase := phaseCounter
        }
      } else {
        when(freq.get.in(0)._1.fire()) {
          phaseCounter := freq.get.in(0)._1.bits.data.asUInt()
        }
      }

      
      val skidInLast = Wire(Flipped(DecoupledIO(Bool())))
      val skidOutLast = Wire(DecoupledIO(Bool()))
      skidOutLast.ready := ioout.ready
      skidInLast.bits := ShiftRegister(freq.get.in(0)._1.bits.last && freq.get.in(0)._1.fire(), 0, false.B)
      skidInLast.valid := freq.get.in(0)._1.valid
      Skid2(1, skidInLast, skidOutLast) := skidInLast.bits
      val lastOut = RegInit(Bool(), false.B)
      val indicator = RegInit(Bool(), false.B)
      when(skidOutLast.bits) {indicator := true.B}
      when(skidOutLast.bits && !indicator) {lastOut := true.B}. elsewhen(ioout.bits.last && ioout.valid) {lastOut := false.B}
      ioout.bits.last := lastOut
      
      val queueCounter = RegInit(0.U(2.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      val lastStateQueueCounter = RegInit(0.U(2.W))
      when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
      val lastStateQueueCounterWire = Wire(UInt(2.W))
      lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
      
      val queueCounterPoff = RegInit(0.U(2.W))
      queueCounterPoff := queueCounterPoff +& poff.get.in(0)._1.fire() -& poff.get.in(0)._1.fire()

      val inFire = RegInit(Bool(), false.B)
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      poff.get.in(0)._1.ready := (queueCounterPoff < latency.U) || (queueCounterPoff === latency.U && ioout.ready)
      ioout.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U))) && ioout.ready)
      
      val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

      val outputBufferSin2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      if (params.useMultiplier) {
        when(inFire){
          when(!enableMultiplying) {
            outputBufferSin := phaseConverter.io.sinOut
            outputBufferCos := phaseConverter.io.cosOut
            outputBufferSin2 := outputBufferSin
            outputBufferCos2 := outputBufferCos
          }.otherwise {
            DspContext.withBinaryPointGrowth(0) {
              outputBufferSin := (phaseConverter.io.sinOut.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferCos := (phaseConverter.io.cosOut.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferSin2 := (outputBufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferCos2 := (outputBufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            }
          }
        }
      } else {
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
          outputBufferSin2 := outputBufferSin
          outputBufferCos2 := outputBufferCos
        }
      }

      when(queueCounter === 2.U){
        when(freq.get.in(0)._1.fire() && inFire){
          ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
        }.elsewhen(freq.get.in(0)._1.fire() && !inFire){
          ioout.bits.data := Cat(outputBufferCos2.asUInt(), outputBufferSin2.asUInt())
        }.elsewhen(!freq.get.in(0)._1.fire() && inFire){
          ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
        }.otherwise{
          ioout.bits.data := Cat(outputBufferCos2.asUInt(), outputBufferSin2.asUInt())
        }
      }.otherwise{
        ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
      }
    } 
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // pinc streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType == Streaming && params.poffType != Streaming) {
        
      val poffReg = RegInit(0.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      
      if ((params.poffType == Config) && params.useMultiplier) {
        val fields = Seq(
        RegField(1, enableMultiplying,
          RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
        RegField(params.protoOut.getWidth, multiplyingFactor,
          RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
        RegField(beatBytes*4, poffReg,   
          RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.poffType == Config) {
        val fields = Seq(
        RegField(beatBytes*4, poffReg,   
          RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.useMultiplier) {
        val fields = Seq(
        RegField(1, enableMultiplying,
          RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
        RegField(params.protoOut.getWidth, multiplyingFactor,
          RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }

      
      val latency = 2 //if (params.syncROMEnable) 2 else 1
      
      // phase counter
      if (params.phaseAccEnable) {
        when(freq.get.in(0)._1.fire()) {
          phaseCounter := phaseCounter + freq.get.in(0)._1.bits.data.asUInt()
        }
      } else {
        when(freq.get.in(0)._1.fire()) {
          phaseCounter := freq.get.in(0)._1.bits.data.asUInt()
        }
      }

      // phase converter
      phaseConverter.io.phase := phaseCounter + poffReg
      
      val skidInLast = Wire(Flipped(DecoupledIO(Bool())))
      val skidOutLast = Wire(DecoupledIO(Bool()))
      skidOutLast.ready := ioout.ready
      skidInLast.bits := ShiftRegister(freq.get.in(0)._1.bits.last && freq.get.in(0)._1.fire(), 0, false.B)
      skidInLast.valid := freq.get.in(0)._1.valid
      Skid2(1, skidInLast, skidOutLast) := skidInLast.bits
      val lastOut = RegInit(Bool(), false.B)
      val indicator = RegInit(Bool(), false.B)
      when(skidOutLast.bits) {indicator := true.B}
      when(skidOutLast.bits && !indicator) {lastOut := true.B}. elsewhen(ioout.bits.last && ioout.valid) {lastOut := false.B}
      ioout.bits.last := lastOut
      
      val queueCounter = RegInit(0.U(2.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      val lastStateQueueCounter = RegInit(0.U(2.W))
      when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
      val lastStateQueueCounterWire = Wire(UInt(2.W))
      lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
      
      val inFire = RegInit(Bool(), false.B)
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      ioout.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U))) && ioout.ready)
      
      val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

      val outputBufferSin2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      if (params.useMultiplier) {
        when(inFire){
          when(!enableMultiplying) {
            outputBufferSin := phaseConverter.io.sinOut
            outputBufferCos := phaseConverter.io.cosOut
            outputBufferSin2 := outputBufferSin
            outputBufferCos2 := outputBufferCos
          }.otherwise {
            DspContext.withBinaryPointGrowth(0) {
              outputBufferSin := (phaseConverter.io.sinOut.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferCos := (phaseConverter.io.cosOut.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferSin2 := (outputBufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
              outputBufferCos2 := (outputBufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* multiplyingFactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            }
          }
        }
      } else {
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
          outputBufferSin2 := outputBufferSin
          outputBufferCos2 := outputBufferCos
        }
      }

      when(queueCounter === 2.U){
        when(freq.get.in(0)._1.fire() && inFire){
          ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
        }.elsewhen(freq.get.in(0)._1.fire() && !inFire){
          ioout.bits.data := Cat(outputBufferCos2.asUInt(), outputBufferSin2.asUInt())
        }.elsewhen(!freq.get.in(0)._1.fire() && inFire){
          ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
        }.otherwise{
          ioout.bits.data := Cat(outputBufferCos2.asUInt(), outputBufferSin2.asUInt())
        }
      }.otherwise{
        ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
      }
    }
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // poff streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType != Streaming && params.poffType == Streaming) {
      
      val outputReady =  ioout.ready
        
      val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
      // registers
      val inputEnableReg = RegInit(true.B)
      val freqReg = RegInit(freqInit.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      
      val enable = outputReady && inputEnableReg

      ioout.valid     := Mux(ioout.ready, RegNext(enable), ioout.ready)

      // phase counter
      if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
        when(outputReady && inputEnableReg && ioout.ready) {
          phaseCounter := phaseCounter + freqReg
        }
      } 
      else {
        phaseCounter := freqReg
      }
      
      // regmap
      if ((params.pincType == Config) && params.useMultiplier) {
        val fields = Seq(
          RegField(1, enableMultiplying,
            RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
          RegField(params.protoOut.getWidth, multiplyingFactor,
            RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
          RegField(beatBytes*4, freqReg,   
            RegFieldDesc(name = "freq",    desc = "nco frequency control"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if ((params.pincType == Fixed) && params.useMultiplier) { 
        val fields = Seq(
          RegField(1, enableMultiplying,
            RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
          RegField(params.protoOut.getWidth, multiplyingFactor,
            RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
          RegField(1, inputEnableReg,   
            RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.pincType == Config) {
        val fields = Seq(
          RegField(beatBytes*4, freqReg,   
            RegFieldDesc(name = "freq",    desc = "nco frequency control"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.pincType == Fixed) {
        val fields = Seq(
          RegField(1, inputEnableReg,   
            RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.useMultiplier) {
        val fields = Seq(
          RegField(1, enableMultiplying,
            RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
          RegField(params.protoOut.getWidth, multiplyingFactor,
            RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }

      // phase converter
      when(poff.get.in(0)._1.fire()){
        phaseConverter.io.phase := phaseCounter + poff.get.in(0)._1.bits.data.asUInt()
      }.otherwise{
        phaseConverter.io.phase := phaseCounter
      }

      poff.get.in(0)._1.ready := RegNext(ioout.ready, false.B)
      
      ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())

    }
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // no streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else {
    
      ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
      
      val outputReady = ioout.ready
          
      val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
      val freqReg = RegInit(freqInit.U((beatBytes*4).W))
      val inputEnableReg = RegInit(true.B)
      val poffReg = RegInit(0.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      
      /*val fields = Seq(
      RegField(1, enableMultiplying,
        RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
      RegField(params.protoOut.getWidth, multiplyingFactor,
        RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
      )*/
      
      val enable = outputReady && inputEnableReg
      ioout.valid     := Mux(ioout.ready, RegNext(enable), ioout.ready)

      // phase counter
      if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
        when(outputReady && inputEnableReg && ioout.ready) {
          phaseCounter := phaseCounter + freqReg
        }
      } 
      else {
        phaseCounter := freqReg
      }

        // phase converter
      phaseConverter.io.phase := phaseCounter + poffReg

      // generate regmap
      if (params.pincType == Config && params.poffType == Config) {
        val fields = Seq(RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
                          RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      else if (params.pincType == Fixed && params.poffType == Config) {
        val fields = Seq(RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
                          RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      else if (params.pincType == Config && params.poffType == Fixed) {
        val fields = Seq(RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")))
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      else if (params.poffType == Config) {
        val fields = Seq(RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      else if (params.pincType == Fixed) {
        val fields = Seq(RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")))
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
    }
  }
}


class AXI4NCOLazyModuleBlock[T <: Data : Real : BinaryRepresentation](params: NCOParams[T], address: AddressSet, beatBytes: Int)(implicit p: Parameters) extends NCOLazyModuleBlock[T](params, beatBytes) {
    //val mem = if (params.pincType == Fixed || params.pincType == Config || params.poffType == Config) Some(AXI4RegisterNode(address = address, beatBytes = beatBytes)) else None
    //override def regmap(mapping: (Int, Seq[RegField])*): Unit = if (params.pincType == Fixed || params.pincType == Config || params.poffType == Config) mem.get.regmap(mapping:_*) else None
    val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
    override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}


object NCOLazyModuleApp extends App
{
    
  trait AXI4Block extends DspBlock[
  AXI4MasterPortParameters,
  AXI4SlavePortParameters,
  AXI4EdgeParameters,
  AXI4EdgeParameters,
  AXI4Bundle] {
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { 
      m => {
        val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
        m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
        val ioMem = InModuleBody { ioMemNode.makeIO() }
        ioMem
      }
    }
    // generate out stream
    val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
    ioStreamNode := 
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
    val outStream = InModuleBody { ioStreamNode.makeIO() }
  }
    
    
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
    poffType = Config,
    useMultiplier = false
  )
    val beatBytes = 4

  implicit val p: Parameters = Parameters.empty

  val ncoModule = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = beatBytes) with AXI4Block {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "NCOLazyModuleApp"), ()=> ncoModule.module) // generate verilog code
}

