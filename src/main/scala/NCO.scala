package nco

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dsptools.numbers._
import dsptools._

abstract class InterfaceType
case object Fixed extends InterfaceType
case object Streaming extends InterfaceType
case object Config extends InterfaceType

case class NCOParams[T <: Data]
(
  phaseWidth: Int,
  tableSize: Int,
  phaseConv: UInt => T,
  protoFreq: UInt,
  protoOut: T,
  protoTable: Option[T] = None,
  nInterpolationTerms: Int = 0,
  rasterizedMode: Boolean,
  ditherEnable: Boolean,
  syncROMEnable: Boolean,
  phaseAccEnable: Boolean,
  roundingMode: TrimType = RoundHalfUp,
  pincType: InterfaceType = Streaming,
  poffType: InterfaceType = Streaming
) {
  requireIsChiselType(protoFreq)
  requireIsChiselType(protoOut)

  val getProtoTable = protoTable.getOrElse(protoOut)
  val tableParams = NCOTableParams(
  phaseWidth = phaseWidth,
  phaseConv = phaseConv,
  protoTable = getProtoTable,
  protoOut = protoOut,
  nInterpolationTerms = nInterpolationTerms,
  rasterizedMode = rasterizedMode,
  tableSize = tableSize,
  ditherEnable = ditherEnable,
  syncROMEnable = syncROMEnable,
  roundingMode = roundingMode
)
}

object FixedNCOParams {
  def apply(tableSize: Int, tableWidth: Int, phaseWidth: Int = 8, rasterizedMode: Boolean = false, nInterpolationTerms: Int = 0, ditherEnable: Boolean = false, syncROMEnable: Boolean = false, phaseAccEnable: Boolean = true, roundingMode: TrimType = RoundHalfUp, pincType: InterfaceType = Streaming, poffType: InterfaceType = Streaming): NCOParams[FixedPoint] = {
    require(tableWidth >= 3)
    val phaseW = {
      if (rasterizedMode){
        (log2Ceil(tableSize)+2).toInt
      } else {
        phaseWidth
      }
    }
    NCOParams(
      phaseWidth = phaseW,
      tableSize  = tableSize,
      phaseConv = {x: UInt => Cat(0.U, x).asTypeOf(FixedPoint.apply(width = phaseWidth.W, binaryPoint = phaseWidth.BP))},
      protoTable = Some(FixedPoint((tableWidth).W, (tableWidth-2).BP)),
      protoOut = FixedPoint((tableWidth).W, (tableWidth-2).BP),
      protoFreq   = UInt(phaseW.W),
      rasterizedMode = rasterizedMode,
      nInterpolationTerms = nInterpolationTerms,
      ditherEnable = ditherEnable,
      syncROMEnable = syncROMEnable,
      phaseAccEnable = phaseAccEnable,
      roundingMode = roundingMode,
      pincType = pincType,
      poffType = poffType
    )
  }
}

object DspRealNCOParams {
  def apply(tableSize: Int, rasterizedMode: Boolean = false, phaseAccEnable: Boolean = true, pincType: InterfaceType = Streaming, poffType: InterfaceType = Streaming): NCOParams[DspReal] = {
    val phaseW = (log2Ceil(tableSize)+2).toInt
    NCOParams(
      phaseWidth = phaseW,
      phaseConv = {x: UInt => (0.U).asReal()},
      protoTable = Some(DspReal()),
      protoOut   = DspReal(),
      protoFreq = UInt(phaseW.W),
      tableSize  = tableSize,
      rasterizedMode = rasterizedMode,
      nInterpolationTerms = 0,
      ditherEnable = false,
      syncROMEnable = false,
      phaseAccEnable = phaseAccEnable,
      roundingMode = RoundHalfUp,
      pincType = pincType,
      poffType = poffType
    )
  }
}


class NCOIOStreamingPINCandPOFF[T <: Data : Real](params: NCOParams[T]) extends Bundle {

  val freq = Flipped(Decoupled(params.protoFreq))
  val poff = Flipped(Decoupled(params.protoFreq))
  val out  = Decoupled(DspComplex(params.protoOut, params.protoOut))
  val inputEnable = None
}

class NCOIOStreamingPINC[T <: Data : Real](params: NCOParams[T]) extends Bundle {

  val freq = Flipped(Decoupled(params.protoFreq))
  val out  = Decoupled(DspComplex(params.protoOut, params.protoOut))
  val poff = if (params.poffType == Config) Some(Input(params.protoFreq)) else None
  val inputEnable = None
}

class NCOIOStreamingPOFF[T <: Data : Real](params: NCOParams[T]) extends Bundle {

  val poff = Flipped(Decoupled(params.protoFreq))
  val out  = Decoupled(DspComplex(params.protoOut, params.protoOut))
  val freq = if (params.pincType == Config) Some(Input(params.protoFreq)) else None
  val inputEnable = if (params.pincType == Fixed) Some(Input(Bool())) else None
}

class NCOIONoStreaming[T <: Data : Real](params: NCOParams[T]) extends Bundle {

  val freq = if (params.pincType == Config) Some(Input(params.protoFreq)) else None
  val poff = if (params.poffType == Config) Some(Input(params.protoFreq)) else None
  val out  = Decoupled(DspComplex(params.protoOut, params.protoOut))
  val inputEnable = if (params.pincType == Fixed) Some(Input(Bool())) else None
}


class NCOStreamingPINCandPOFF[T <: Data : Real : BinaryRepresentation](params: NCOParams[T]) extends Module {

  val io = IO(new NCOIOStreamingPINCandPOFF(params))
  val tableSize = params.tableSize

  val phaseCounter = RegInit(UInt(params.phaseWidth.W), 0.U)
  val phaseConverter = Module(new NCOTable(params.tableParams))

  if (params.phaseAccEnable) {
    when(io.freq.fire()) {
      phaseCounter := phaseCounter + io.freq.bits.asUInt()
    }
    when (io.poff.fire()) {
      phaseConverter.io.phase := phaseCounter + io.poff.bits.asUInt()
    }.otherwise {
      phaseConverter.io.phase := phaseCounter
    }
  } else {
    when(io.freq.fire()) {
      phaseCounter := io.freq.bits.asUInt()
    }
    phaseConverter.io.phase := phaseCounter
  }

  val latency = if (params.syncROMEnable) 2 else 1
  
  val queueCounter = RegInit(0.U(2.W))
  queueCounter := queueCounter +& io.freq.fire() -& io.out.fire()
  val lastStateQueueCounter = RegInit(0.U(2.W))
  when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
  val lastStateQueueCounterWire = Wire(UInt(2.W))
  lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
  
  val queueCounterPoff = RegInit(0.U(2.W))
  queueCounterPoff := queueCounterPoff +& io.poff.fire() -& io.poff.fire()
  
  val outFire = RegInit(Bool(), false.B)
  val inFire = RegInit(Bool(), false.B)
  when(io.out.fire()) {outFire := true.B}.otherwise {outFire := false.B}
  when(io.freq.fire()) {inFire := true.B}.otherwise {inFire := false.B}
  
  io.freq.ready := (queueCounter < latency.U) || (queueCounter === latency.U && io.out.ready)
  io.poff.ready := (queueCounterPoff < latency.U) || (queueCounterPoff === latency.U && io.out.ready)
  if(!params.syncROMEnable) {
      io.out.valid := ((queueCounter === 1.U) && io.out.ready)
  } else {
      io.out.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U)) && inFire) && io.out.ready)
  }
  
  val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
  val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

  if(!params.syncROMEnable){
    when(inFire){
      outputBufferSin := phaseConverter.io.sinOut
      outputBufferCos := phaseConverter.io.cosOut
    }
    when(queueCounter === 1.U){
      when(io.freq.fire() && inFire){
        io.out.bits.real := phaseConverter.io.cosOut
        io.out.bits.imag := phaseConverter.io.sinOut
      }.elsewhen(io.freq.fire() && !inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.elsewhen(!io.freq.fire() && inFire){
        io.out.bits.real := phaseConverter.io.cosOut
        io.out.bits.imag := phaseConverter.io.sinOut
      }.otherwise{
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }
    }.otherwise{
      io.out.bits.real := phaseConverter.io.cosOut
      io.out.bits.imag := phaseConverter.io.sinOut
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
      when(io.freq.fire() && inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.elsewhen(io.freq.fire() && !inFire){
        io.out.bits.real := outputBufferCos2
        io.out.bits.imag := outputBufferSin2
      }.elsewhen(!io.freq.fire() && inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.otherwise{
        io.out.bits.real := outputBufferCos2
        io.out.bits.imag := outputBufferSin2
      }
    }.otherwise{
      io.out.bits.real := outputBufferCos
      io.out.bits.imag := outputBufferSin
    }
  }
}


class NCOStreamingPINC[T <: Data : Real : BinaryRepresentation](params: NCOParams[T]) extends Module {

  val io = IO(new NCOIOStreamingPINC(params))
  val tableSize = params.tableSize

  val phaseCounter = RegInit(UInt(params.phaseWidth.W), 0.U)
  val phaseConverter = Module(new NCOTable(params.tableParams))


  if (params.phaseAccEnable) {
    when(io.freq.fire()) {
      phaseCounter := phaseCounter + io.freq.bits.asUInt()
    }
  } else {
    when(io.freq.fire()) {
      phaseCounter := io.freq.bits.asUInt()
    }
  }

  phaseConverter.io.phase := phaseCounter + io.poff.getOrElse(0.U)
  
  val latency = if (params.syncROMEnable) 2 else 1
  
  val queueCounter = RegInit(0.U(2.W))
  queueCounter := queueCounter +& io.freq.fire() -& io.out.fire()
  val lastStateQueueCounter = RegInit(0.U(2.W))
  when (queueCounter =/= RegNext(queueCounter)) {lastStateQueueCounter := RegNext(queueCounter)}
  val lastStateQueueCounterWire = Wire(UInt(2.W))
  lastStateQueueCounterWire := Mux(queueCounter =/= RegNext(queueCounter), RegNext(queueCounter), lastStateQueueCounter)
  
  val outFire = RegInit(Bool(), false.B)
  val inFire = RegInit(Bool(), false.B)
  when(io.out.fire()) {outFire := true.B}.otherwise {outFire := false.B}
  when(io.freq.fire()) {inFire := true.B}.otherwise {inFire := false.B}
  
  io.freq.ready := (queueCounter < latency.U) || (queueCounter === latency.U && io.out.ready)
  if(!params.syncROMEnable) {
      io.out.valid := ((queueCounter === 1.U) && io.out.ready)
  } else {
      io.out.valid := (((queueCounter === 2.U) || ((queueCounter === 1.U) && (lastStateQueueCounterWire === 2.U)) && inFire) && io.out.ready)
  }
  
  val outputBufferSin = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))
  val outputBufferCos = RegInit(params.protoOut, 0.U.asTypeOf(params.protoOut))

  if(!params.syncROMEnable){
    when(inFire){
      outputBufferSin := phaseConverter.io.sinOut
      outputBufferCos := phaseConverter.io.cosOut
    }
    when(queueCounter === 1.U){
      when(io.freq.fire() && inFire){
        io.out.bits.real := phaseConverter.io.cosOut
        io.out.bits.imag := phaseConverter.io.sinOut
      }.elsewhen(io.freq.fire() && !inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.elsewhen(!io.freq.fire() && inFire){
        io.out.bits.real := phaseConverter.io.cosOut
        io.out.bits.imag := phaseConverter.io.sinOut
      }.otherwise{
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }
    }.otherwise{
      io.out.bits.real := phaseConverter.io.cosOut
      io.out.bits.imag := phaseConverter.io.sinOut
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
      when(io.freq.fire() && inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.elsewhen(io.freq.fire() && !inFire){
        io.out.bits.real := outputBufferCos2
        io.out.bits.imag := outputBufferSin2
      }.elsewhen(!io.freq.fire() && inFire){
        io.out.bits.real := outputBufferCos
        io.out.bits.imag := outputBufferSin
      }.otherwise{
        io.out.bits.real := outputBufferCos2
        io.out.bits.imag := outputBufferSin2
      }
    }.otherwise{
      io.out.bits.real := outputBufferCos
      io.out.bits.imag := outputBufferSin
    }
  }
}


class NCOStreamingPOFF[T <: Data : Real : BinaryRepresentation](params: NCOParams[T]) extends Module {

  val io = IO(new NCOIOStreamingPOFF(params))
  val tableSize = params.tableSize

  val phaseCounter = RegInit(UInt(params.phaseWidth.W), 0.U)
  val phaseConverter = Module(new NCOTable(params.tableParams))
  val outputReady = {
    if (params.syncROMEnable) {
      RegNext(RegNext(io.out.ready, false.B))
    } else {
      RegNext(io.out.ready, false.B)
    }
  }

  val enable = outputReady && io.inputEnable.getOrElse(true.B)

  if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
    when(enable && io.out.ready) {
      phaseCounter := phaseCounter + io.freq.getOrElse(1.U)
    }
  } else {
    phaseCounter := io.freq.getOrElse(0.U)
  }
  

  when(io.poff.fire()){
    phaseConverter.io.phase := phaseCounter + io.poff.bits
  }.otherwise{
    phaseConverter.io.phase := phaseCounter
  }

  io.out.bits.real := phaseConverter.io.cosOut
  io.out.bits.imag := phaseConverter.io.sinOut

  io.out.valid     := Mux(io.out.ready, RegNext(enable), io.out.ready)

  io.poff.ready := RegNext(io.out.ready, false.B)

}


class NCONoStreaming[T <: Data : Real : BinaryRepresentation](params: NCOParams[T]) extends Module {

  val io = IO(new NCOIONoStreaming(params))
  val tableSize = params.tableSize

  val phaseCounter = RegInit(UInt(params.phaseWidth.W), 0.U)
  val phaseConverter = Module(new NCOTable(params.tableParams))
  val outputReady = {
    if (params.syncROMEnable) {
      RegNext(RegNext(io.out.ready, false.B))
    } else {
      RegNext(io.out.ready, false.B)
    }
  }
  val enable = outputReady && io.inputEnable.getOrElse(true.B)

  if ((params.phaseAccEnable) || (!(params.phaseAccEnable) && (params.pincType == Fixed))) {
    when(enable && io.out.ready) {
      phaseCounter := phaseCounter + io.freq.getOrElse(1.U)
    }
  } else {
    phaseCounter := io.freq.getOrElse(0.U)
  }

  phaseConverter.io.phase := phaseCounter + io.poff.getOrElse(0.U)

  io.out.bits.real := phaseConverter.io.cosOut
  io.out.bits.imag := phaseConverter.io.sinOut

  io.out.valid     := Mux(io.out.ready, RegNext(enable), io.out.ready)

}
