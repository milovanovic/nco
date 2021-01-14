// SPDX-License-Identifier: Apache-2.0

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
  
  val inQAM = if (params.useQAM && (params.pincType == Streaming)) Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters())) else None

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
      val enableQAM = RegInit(Bool(), false.B)
      
      if (params.useMultiplier && !params.useQAM) {
        val fields = Seq(
        RegField(1, enableMultiplying,
          RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
        RegField(params.protoOut.getWidth, multiplyingFactor,
          RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.useQAM && !params.useMultiplier) {
        val fields = Seq(
        RegField(1, enableQAM,
          RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      } else if (params.useQAM && params.useMultiplier) {
        val fields = Seq(
        RegField(1, enableMultiplying,
          RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
        RegField(params.protoOut.getWidth, multiplyingFactor,
          RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
        RegField(1, enableQAM,
          RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
        )
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
      }
      
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
      
      val queueCounter = RegInit(0.U(4.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      
      val queueCounterPoff = RegInit(0.U(4.W))
      queueCounterPoff := queueCounterPoff +& poff.get.in(0)._1.fire() -& ioout.fire()
      
      val latency = {
        if (!params.syncROMEnable) {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 2)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 3) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 3)
          else 2
        } else {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 3)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 4) // ili + 3
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 4)
          else 3
        }
      }

      val inFire = RegInit(Bool(), false.B)
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      poff.get.in(0)._1.ready := (queueCounterPoff < latency.U) || (queueCounterPoff === latency.U && ioout.ready)
      
      val bufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val bufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      bufferSin := phaseConverter.io.sinOut
      bufferCos := phaseConverter.io.cosOut
      
      if (params.useMultiplier && !params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        
        val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }

        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos2.asUInt(), bufferSin2.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
        
      } else if (params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        bufferSin2 := DontCare
        bufferCos2 := DontCare
        
        if (params.useMultiplier) {
          val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
            bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          }
        }
        
        val queueCounterQAM = RegInit(0.U(4.W))
        queueCounterQAM := queueCounterQAM +& inQAM.get.in(0)._1.fire() -& ioout.fire()
        
        inQAM.get.in(0)._1.ready := (queueCounterQAM < latency.U) || (queueCounterQAM === latency.U && ioout.ready)
        
        val QAMin = RegInit(UInt((8*beatBytes).W), 0.U)
        val iinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
        val qinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
        
        val IOUT1 = Wire(params.protoOut)
        val QOUT1 = Wire(params.protoOut)
        val IOUT2 = Wire(params.protoOut)
        val QOUT2 = Wire(params.protoOut)
        
        val IOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val QOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        val ifactor = Wire(UInt((beatBytes*4).W))
        val qfactor = Wire(UInt((beatBytes*4).W))
          
        when (inQAM.get.in(0)._1.fire()) {
          QAMin := inQAM.get.in(0)._1.bits.data
        }
        if (params.syncROMEnable) {
          iinQAM := RegNext(QAMin(8*beatBytes-1, 4*beatBytes))
          qinQAM := RegNext(QAMin(4*beatBytes-1, 0))
        } else {
          iinQAM := QAMin(8*beatBytes-1, 4*beatBytes)
          qinQAM := QAMin(4*beatBytes-1, 0)
        }
        ifactor := Mux(enableQAM, iinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        qfactor := Mux(enableQAM, qinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        
        val sinQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferSin2, bufferSin) else bufferSin
        val cosQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferCos2, bufferCos) else bufferCos
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          IOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          IOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        when (enableQAM) {
          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, trimType = params.roundingMode)) {
            IOUT := (IOUT2 context_- IOUT1).asTypeOf(params.protoOut)
            QOUT := (QOUT2 context_+ QOUT1).asTypeOf(params.protoOut)
          }
        } .otherwise {
          IOUT := IOUT2.asTypeOf(params.protoOut)
          QOUT := QOUT1.asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(IOUT.asUInt(), QOUT.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
        
      } else {
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos.asUInt(), bufferSin.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      }
      
      val queueLast = Module(new Queue(Bool(), latency+1, flow = true))
      queueLast.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
      queueLast.io.enq.bits := ShiftRegister(freq.get.in(0)._1.bits.last, latency, resetData = false.B, en = true.B)
      queueLast.io.deq.ready := ioout.ready
      ioout.bits.last := queueLast.io.deq.bits
    }
    
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // pinc streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType == Streaming && params.poffType != Streaming) {
        
      val poffReg = RegInit(0.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      val enableQAM = RegInit(Bool(), false.B)
      
      if (!params.useQAM) {
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
      } else {
        if ((params.poffType == Config) && params.useMultiplier) {
          val fields = Seq(
          RegField(1, enableMultiplying,
            RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
          RegField(params.protoOut.getWidth, multiplyingFactor,
            RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
          RegField(beatBytes*4, poffReg,   
            RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
          RegField(1, enableQAM,
            RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.poffType == Config) {
          val fields = Seq(
          RegField(beatBytes*4, poffReg,   
            RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
          RegField(1, enableQAM,
            RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.useMultiplier) {
          val fields = Seq(
          RegField(1, enableMultiplying,
            RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
          RegField(params.protoOut.getWidth, multiplyingFactor,
            RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
          RegField(1, enableQAM,
            RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else {
          val fields = Seq(
          RegField(1, enableQAM,
            RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
      }
      
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
      
      val latency = {
        if (!params.syncROMEnable) {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 2)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 3) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 3)
          else 2
        } else {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 3)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 4) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 4)
          else 3
        }
      }
      
      val inFire = RegInit(Bool(), false.B)
      when(freq.get.in(0)._1.fire()) {inFire := true.B}.otherwise {inFire := false.B}
      
      val queueCounter = RegInit(0.U(4.W))
      queueCounter := queueCounter +& freq.get.in(0)._1.fire() -& ioout.fire()
      
      freq.get.in(0)._1.ready := (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      
      val bufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val bufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      bufferSin := phaseConverter.io.sinOut
      bufferCos := phaseConverter.io.cosOut
      
      if (params.useMultiplier && !params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        
        val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }

        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos2.asUInt(), bufferSin2.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
        
      } else if (params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        bufferSin2 := DontCare
        bufferCos2 := DontCare
        
        if (params.useMultiplier) {
          val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
            bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          }
        }
        
        val queueCounterQAM = RegInit(0.U(4.W))
        queueCounterQAM := queueCounterQAM +& inQAM.get.in(0)._1.fire() -& ioout.fire()
        
        inQAM.get.in(0)._1.ready := (queueCounterQAM < latency.U) || (queueCounterQAM === latency.U && ioout.ready)
        
        val QAMin = RegInit(UInt((8*beatBytes).W), 0.U)
        val iinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
        val qinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
        
        val IOUT1 = Wire(params.protoOut)
        val QOUT1 = Wire(params.protoOut)
        val IOUT2 = Wire(params.protoOut)
        val QOUT2 = Wire(params.protoOut)
        
        val IOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val QOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        val ifactor = Wire(UInt((beatBytes*4).W))
        val qfactor = Wire(UInt((beatBytes*4).W))
          
        when (inQAM.get.in(0)._1.fire()) {
          QAMin := inQAM.get.in(0)._1.bits.data
        }
        if (params.syncROMEnable) {
          iinQAM := RegNext(QAMin(8*beatBytes-1, 4*beatBytes))
          qinQAM := RegNext(QAMin(4*beatBytes-1, 0))
        } else {
          iinQAM := QAMin(8*beatBytes-1, 4*beatBytes)
          qinQAM := QAMin(4*beatBytes-1, 0)
        }
        ifactor := Mux(enableQAM, iinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        qfactor := Mux(enableQAM, qinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        
        val sinQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferSin2, bufferSin) else bufferSin
        val cosQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferCos2, bufferCos) else bufferCos
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          IOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          IOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        when (enableQAM) {
          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, trimType = params.roundingMode)) {
            IOUT := (IOUT2 context_- IOUT1).asTypeOf(params.protoOut)
            QOUT := (QOUT2 context_+ QOUT1).asTypeOf(params.protoOut)
          }
        } .otherwise {
          IOUT := IOUT2.asTypeOf(params.protoOut)
          QOUT := QOUT1.asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(IOUT.asUInt(), QOUT.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
        
      } else {
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos.asUInt(), bufferSin.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      }
      
      val queueLast = Module(new Queue(Bool(), latency+1, flow = true))
      queueLast.io.enq.valid := ShiftRegister(freq.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
      queueLast.io.enq.bits := ShiftRegister(freq.get.in(0)._1.bits.last, latency, resetData = false.B, en = true.B)
      queueLast.io.deq.ready := ioout.ready
      ioout.bits.last := queueLast.io.deq.bits
    }
    
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // poff streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else if (params.pincType != Streaming && params.poffType == Streaming) {
        
      val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
      
      // registers
      val inputEnableReg = RegInit(false.B)
      val freqReg = RegInit(freqInit.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      val enableQAM = RegInit(Bool(), false.B)
      val iinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
      val qinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
      
      // regmap
      if (!params.useQAM) {
        if ((params.pincType == Config) && params.useMultiplier) {
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
            RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(beatBytes*4, freqReg, RegFieldDesc(name = "freq",    desc = "nco frequency control"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if ((params.pincType == Fixed) && params.useMultiplier) { 
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
            RegField(1, inputEnableReg, RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.pincType == Config) {
          val fields = Seq(
            RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(beatBytes*4, freqReg, RegFieldDesc(name = "freq",    desc = "nco frequency control"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.pincType == Fixed) {
          val fields = Seq(
            RegField(1, inputEnableReg, RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.useMultiplier) {
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
      } else {
        if ((params.pincType == Config) && params.useMultiplier) {
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
            RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(beatBytes*4, freqReg, RegFieldDesc(name = "freq",    desc = "nco frequency control")),
            RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
            RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
            RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if ((params.pincType == Fixed) && params.useMultiplier) { 
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
            RegField(1, inputEnableReg, RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
            RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
            RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.pincType == Config) {
          val fields = Seq(
            RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(beatBytes*4, freqReg, RegFieldDesc(name = "freq",    desc = "nco frequency control")),
            RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
            RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
            RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.pincType == Fixed) {
          val fields = Seq(
            RegField(1, inputEnableReg, RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
            RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
            RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
            RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else if (params.useMultiplier) {
          val fields = Seq(
            RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
            RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
            RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
            RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
            RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        } else {
          val fields = Seq(
          RegField(1, enableQAM,RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
          RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
          RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
          )
          regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
        }
      }

      val latency = {
        if (!params.syncROMEnable) {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 2)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 3) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 3)
          else 2
        } else {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 3)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 4) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 4)
          else 3
        }
      }
      val queueCounter = RegInit(0.U(4.W))
      val inReady = (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
      queueCounter := queueCounter +& (inReady && inputEnableReg) -& ioout.fire()
      
      // phase counter
      if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
        when(inputEnableReg && inReady) {
          phaseCounter := phaseCounter + freqReg
        }
      } else {
        when(inputEnableReg && inReady) {
          phaseCounter := freqReg
        }
      }
      
      // phase converter
      when(poff.get.in(0)._1.fire()){
        phaseConverter.io.phase := phaseCounter + poff.get.in(0)._1.bits.data.asUInt()
      }.otherwise{
        phaseConverter.io.phase := phaseCounter
      }
      
      poff.get.in(0)._1.ready := inReady
      
      val bufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val bufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      bufferSin := phaseConverter.io.sinOut
      bufferCos := phaseConverter.io.cosOut
      
      if (params.useMultiplier && !params.useQAM) {

        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        
        val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos2.asUInt(), bufferSin2.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      
      } else if (params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        bufferSin2 := DontCare
        bufferCos2 := DontCare
        
        if (params.useMultiplier) {
          val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
            bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          }
        }
        
        val IOUT1 = Wire(params.protoOut)
        val QOUT1 = Wire(params.protoOut)
        val IOUT2 = Wire(params.protoOut)
        val QOUT2 = Wire(params.protoOut)
        
        val IOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val QOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        val ifactor = Wire(UInt((beatBytes*4).W))
        val qfactor = Wire(UInt((beatBytes*4).W))
          
        ifactor := Mux(enableQAM, iinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        qfactor := Mux(enableQAM, qinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        
        val sinQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferSin2, bufferSin) else bufferSin
        val cosQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferCos2, bufferCos) else bufferCos
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          IOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          IOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        when (enableQAM) {
          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, trimType = params.roundingMode)) {
            IOUT := (IOUT2 context_- IOUT1).asTypeOf(params.protoOut)
            QOUT := (QOUT2 context_+ QOUT1).asTypeOf(params.protoOut)
          }
        } .otherwise {
          IOUT := IOUT2.asTypeOf(params.protoOut)
          QOUT := QOUT1.asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(IOUT.asUInt(), QOUT.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      
      } else {
      
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, pipe = false, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos.asUInt(), bufferSin.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      }
      
      val queueLast = Module(new Queue(Bool(), latency+1, flow = true))
      queueLast.io.enq.valid := ShiftRegister(poff.get.in(0)._1.fire(), latency, resetData = false.B, en = true.B)
      queueLast.io.enq.bits := ShiftRegister(poff.get.in(0)._1.bits.last, latency, resetData = false.B, en = true.B)
      queueLast.io.deq.ready := ioout.ready
      ioout.bits.last := queueLast.io.deq.bits
    }
    
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    // no streaming
    //-----------------------------------------------------------------------------------------------------------------------------------------------------
    else {
          
      val freqInit = if((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) 1 else 0
      val freqReg = RegInit(freqInit.U((beatBytes*4).W))
      val inputEnableReg = RegInit(false.B)
      val poffReg = RegInit(0.U((beatBytes*4).W))
      val enableMultiplying = RegInit(Bool(), false.B)
      val multiplyingFactor = RegInit(UInt((beatBytes*4).W), 0.U)
      val enableQAM = RegInit(Bool(), false.B)
      val iinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
      val qinQAM = RegInit(UInt((4*beatBytes).W), 0.U)
      
      // generate regmap
      if (!params.useQAM) {
        if (!params.useMultiplier) {
          if (params.pincType == Config && params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed && params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Config && params.poffType == Fixed) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
        } else {
          if (params.pincType == Config && params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed && params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Config && params.poffType == Fixed) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
        }
      } else {
        if (!params.useMultiplier) {
          if (params.pincType == Config && params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed && params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Config && params.poffType == Fixed) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.poffType == Config) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed) {
            val fields = Seq(
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
        } else {
          if (params.pincType == Config && params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed && params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Config && params.poffType == Fixed) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, freqReg,   RegFieldDesc(name = "freq",    desc = "nco frequency control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.poffType == Config) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(beatBytes*4, poffReg,   RegFieldDesc(name = "poff",    desc = "nco phase offset control")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
          else if (params.pincType == Fixed) {
            val fields = Seq(
              RegField(1, enableMultiplying, RegFieldDesc(name = "enableMultiplying", desc = "enable bit for multiplying")),
              RegField(params.protoOut.getWidth, multiplyingFactor, RegFieldDesc(name = "multiplyingFactor", desc = "multiplying factor")),
              RegField(1, inputEnableReg,   RegFieldDesc(name = "inputEN",    desc = "input enable reg")),
              RegField(1, enableQAM, RegFieldDesc(name = "enableQAM", desc = "enable bit for QAM")),
              RegField(beatBytes*4, iinQAM, RegFieldDesc(name = "iinQAM",    desc = "QAM in-phase input")),
              RegField(beatBytes*4, qinQAM, RegFieldDesc(name = "qinQAM",    desc = "QAM quadrature input"))
            )
            regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
          }
        }
      }
      
      val latency = {
        if (!params.syncROMEnable) {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 2)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 3) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 3)
          else 2
        } else {
          if (params.useMultiplier && !params.useQAM) (params.numMulPipes + 3)
          else if (!params.useMultiplier && params.useQAM) (params.numMulPipes + 4) // ili + 2
          else if (params.useMultiplier && params.useQAM) (2 * params.numMulPipes + 4)
          else 3
        }
      }
      val queueCounter = RegInit(0.U(4.W))
      val inReady = (queueCounter < latency.U) || (queueCounter === latency.U && ioout.ready)
       queueCounter := queueCounter +& (inReady && inputEnableReg) -& ioout.fire()
      
      if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
        when(inputEnableReg && inReady) {
          phaseCounter := phaseCounter + freqReg
        }
      } 
      else {
        when(inputEnableReg && inReady) {
          phaseCounter := freqReg
        }
      }

      // phase converter
      phaseConverter.io.phase := phaseCounter + poffReg
      
      val bufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      val bufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
      
      bufferSin := phaseConverter.io.sinOut
      bufferCos := phaseConverter.io.cosOut
      
      if (params.useMultiplier && !params.useQAM) {

        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        
        val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos2.asUInt(), bufferSin2.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      
       } else if (params.useQAM) {
      
        val bufferSin2 = Wire(params.protoOut)
        val bufferCos2 = Wire(params.protoOut)
        bufferSin2 := DontCare
        bufferCos2 := DontCare
        
        if (params.useMultiplier) {
          val factor = Mux(enableMultiplying, multiplyingFactor, (1.U << (beatBytes*4-2)))

          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
            bufferSin2 := (bufferSin.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
            bufferCos2 := (bufferCos.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* factor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          }
        }
        
        val IOUT1 = Wire(params.protoOut)
        val QOUT1 = Wire(params.protoOut)
        val IOUT2 = Wire(params.protoOut)
        val QOUT2 = Wire(params.protoOut)
        
        val IOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        val QOUT = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
        
        val ifactor = Wire(UInt((beatBytes*4).W))
        val qfactor = Wire(UInt((beatBytes*4).W))
          
        ifactor := Mux(enableQAM, iinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        qfactor := Mux(enableQAM, qinQAM, (1.U << (beatBytes*4-2)).asTypeOf(UInt((beatBytes*4).W)))
        
        val sinQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferSin2, bufferSin) else bufferSin
        val cosQAM = if (params.useMultiplier) Mux(enableMultiplying, bufferCos2, bufferCos) else bufferCos
        
        DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, numMulPipes = params.numMulPipes, trimType = params.roundingMode)) {
          IOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          IOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT1 := (sinQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* ifactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
          QOUT2 := (cosQAM.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP)) context_* qfactor.asTypeOf(FixedPoint((params.protoOut.getWidth).W, (params.protoOut.getWidth-2).BP))).asTypeOf(params.protoOut)
        }
        
        when (enableQAM) {
          DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, trimType = params.roundingMode)) {
            IOUT := (IOUT2 context_- IOUT1).asTypeOf(params.protoOut)
            QOUT := (QOUT2 context_+ QOUT1).asTypeOf(params.protoOut)
          }
        } .otherwise {
          IOUT := IOUT2.asTypeOf(params.protoOut)
          QOUT := QOUT1.asTypeOf(params.protoOut)
        }
        
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(IOUT.asUInt(), QOUT.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      
      } else {
      
        val queue = Module(new Queue(UInt((beatBytes * 8).W), latency+1, pipe = false, flow = true))
        queue.io.enq.valid := ShiftRegister((inReady && inputEnableReg), latency, resetData = false.B, en = true.B)
        queue.io.enq.bits := Cat(bufferCos.asUInt(), bufferSin.asUInt())
        ioout.valid := queue.io.deq.valid && ioout.ready
        queue.io.deq.ready := ioout.ready
        ioout.bits.data := queue.io.deq.bits
      }
      ioout.bits.last := false.B
    }
  }
}


class AXI4NCOLazyModuleBlock[T <: Data : Real : BinaryRepresentation](params: NCOParams[T], address: AddressSet, beatBytes: Int)(implicit p: Parameters) extends NCOLazyModuleBlock[T](params, beatBytes) {
    val mem = if (params.pincType == Fixed || params.pincType == Config || params.poffType == Config || params.useMultiplier || params.useQAM) Some(AXI4RegisterNode(address = address, beatBytes = beatBytes)) else None
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
    tableSize = 64,
    tableWidth = 16,
    phaseWidth = 8,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed,
    useMultiplier = false,
    numMulPipes = 1,
    useQAM = false
  )
    val beatBytes = 4

  implicit val p: Parameters = Parameters.empty

  //val ncoModule = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = beatBytes) with AXI4Block {
  //})
  val ncoModule = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = beatBytes) with AXI4Block {
    val ioparallelin = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    freq.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioparallelin
    val inStreamFreq = InModuleBody { ioparallelin.makeIO() }
    
    /*val ioparallelinPoff = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    poff.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioparallelinPoff
    val inStreamPoff = InModuleBody { ioparallelinPoff.makeIO() }*/
    
    /*val ioparallelinQAM = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    inQAM.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) := ioparallelinQAM
    val inStreamQAM = InModuleBody { ioparallelinQAM.makeIO() }*/
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "NCOLazyModuleApp"), ()=> ncoModule.module) // generate verilog code
}

