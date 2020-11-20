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
import magnitude.Skid



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
  
      val latency = if (params.syncROMEnable) 2 else 1
      
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
      skidInLast.bits := ShiftRegister(freq.get.in(0)._1.bits.last && freq.get.in(0)._1.fire(), latency-1, false.B)
      skidInLast.valid := freq.get.in(0)._1.valid
      Skid(latency, skidInLast, skidOutLast) := skidInLast.bits
      ioout.bits.last := skidOutLast.bits && ioout.valid
      
      val queueCounter = RegInit(0.U(2.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      val lastStateQueueCounter = RegInit(0.U(2.W))
      when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
      val lastStateQueueCounterWire = Wire(UInt(2.W))
      lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
      
      val queueCounterPoff = RegInit(0.U(2.W))
      queueCounterPoff := queueCounterPoff +& poff.get.in(0)._1.fire() -& poff.get.in(0)._1.fire()

      val outFire = RegInit(Bool(), false.B)
      val inFire = RegInit(Bool(), false.B)
      when(ioout.fire()) {outFire := true.B}.otherwise {outFire := false.B}
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      poff.get.in(0)._1.ready := (queueCounterPoff < latency.U) || (queueCounterPoff === latency.U && ioout.ready)
      if(!params.syncROMEnable) {
        ioout.valid := ((queueCounter === 1.U) && ioout.ready)
      } else {
        ioout.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U)) && inFire) && ioout.ready)
      }
      
      val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

      if(!params.syncROMEnable){
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
        }
        when(queueCounter === 1.U){
          when(freq.get.in(0)._1.fire() && inFire){
            ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
          }.elsewhen(freq.get.in(0)._1.fire() && !inFire){
            ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
          }.elsewhen(!freq.get.in(0)._1.fire() && inFire){
            ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
          }.otherwise{
            ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
          }
        }.otherwise{
          ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
        }
      } else {
        val outputBufferSin2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val outputBufferCos2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
          outputBufferSin2 := outputBufferSin
          outputBufferCos2 := outputBufferCos
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
    } 
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // pinc streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType == Streaming && params.poffType != Streaming) {
        
      val poffReg = RegInit(0.U((beatBytes*4).W))
  
      // generate regmap
      if (params.poffType == Config) {
        val fields = Seq(RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))

        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      
      val latency = if (params.syncROMEnable) 2 else 1
      
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
      skidInLast.bits := ShiftRegister(freq.get.in(0)._1.bits.last && freq.get.in(0)._1.fire(), latency-1, false.B)
      skidInLast.valid := freq.get.in(0)._1.valid
      Skid(latency, skidInLast, skidOutLast) := skidInLast.bits
      ioout.bits.last := skidOutLast.bits && ioout.valid
      
      val queueCounter = RegInit(0.U(2.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      val lastStateQueueCounter = RegInit(0.U(2.W))
      when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
      val lastStateQueueCounterWire = Wire(UInt(2.W))
      lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
      
      val outFire = RegInit(Bool(), false.B)
      val inFire = RegInit(Bool(), false.B)
      when(ioout.fire()) {outFire := true.B}.otherwise {outFire := false.B}
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      if(!params.syncROMEnable) {
        ioout.valid := ((queueCounter === 1.U) && ioout.ready)
      } else {
        ioout.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U)) && inFire) && ioout.ready)
      }
      
      val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

      if(!params.syncROMEnable){
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
        }
        when(queueCounter === 1.U){
          when(freq.get.in(0)._1.fire() && inFire){
            ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
          }.elsewhen(freq.get.in(0)._1.fire() && !inFire){
            ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
          }.elsewhen(!freq.get.in(0)._1.fire() && inFire){
            ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
          }.otherwise{
            ioout.bits.data := Cat(outputBufferCos.asUInt(), outputBufferSin.asUInt())
          }
        }.otherwise{
          ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
        }
      } else {
        val outputBufferSin2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val outputBufferCos2 = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        when(inFire){
          outputBufferSin := phaseConverter.io.sinOut
          outputBufferCos := phaseConverter.io.cosOut
          outputBufferSin2 := outputBufferSin
          outputBufferCos2 := outputBufferCos
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
    }
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // poff streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType != Streaming && params.poffType == Streaming) {
        
      ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
      
      val outputReady =  ioout.ready
        
      val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
      // registers
      val inputEnableReg = RegInit(true.B)
      val freqReg = RegInit(freqInit.U((beatBytes*4).W))
      
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

      // generate regmap
      if (params.pincType == Config) {
        val fields = Seq(RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")))

        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      else if (params.pincType == Fixed) {
        val fields = Seq(RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")))

        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }

      // phase converter
      when(poff.get.in(0)._1.fire()){
        phaseConverter.io.phase := phaseCounter + poff.get.in(0)._1.bits.data.asUInt()
      }.otherwise{
        phaseConverter.io.phase := phaseCounter
      }

      poff.get.in(0)._1.ready := RegNext(ioout.ready, false.B)

    }
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // no streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else {
    
        ioout.bits.data := Cat(phaseConverter.io.cosOut.asUInt(), phaseConverter.io.sinOut.asUInt())
        
        // syncrom
        val outputReady = ioout.ready
            
        // registers
        val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
        val freqReg = RegInit(freqInit.U((beatBytes*4).W))
        val inputEnableReg = RegInit(true.B)
        val poffReg = RegInit(0.U((beatBytes*4).W))
        
        val enable = outputReady && inputEnableReg
        //ioout.valid := RegNext(enable, false.B)
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
          
          // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
        else if (params.pincType == Fixed && params.poffType == Config) {
          val fields = Seq(RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
                            RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))
          
          // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
        else if (params.pincType == Config && params.poffType == Fixed) {
          val fields = Seq(RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")))
          
          // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
        else if (params.poffType == Config) {
          val fields = Seq(RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")))
          
          // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
        else if (params.pincType == Fixed) {
          val fields = Seq(RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")))
          
          // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
    }
  }
}


class AXI4NCOLazyModuleBlock[T <: Data : Real : BinaryRepresentation](params: NCOParams[T], address: AddressSet, beatBytes: Int)(implicit p: Parameters) extends NCOLazyModuleBlock[T](params, beatBytes) {
    val mem = if (params.pincType == Fixed || params.pincType == Config || params.poffType == Config) Some(AXI4RegisterNode(address = address, beatBytes = beatBytes)) else None
    override def regmap(mapping: (Int, Seq[RegField])*): Unit = if (params.pincType == Fixed || params.pincType == Config || params.poffType == Config) mem.get.regmap(mapping:_*) else None
    //val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
    //override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
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
    poffType = Config
  )
    val beatBytes = 4

  implicit val p: Parameters = Parameters.empty

  val ncoModule = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = beatBytes) with AXI4Block {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "NCOLazyModuleApp"), ()=> ncoModule.module) // generate verilog code
}

