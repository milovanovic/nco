// SPDX-License-Identifier: Apache-2.0

package nco

import breeze.numerics.sin
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.SyncROM
import dsptools.numbers._
import dsptools._
//import dsptools.{hasContext, DspContext, RoundHalfUp, RoundDown}

import scala.collection.mutable
import scala.math.Pi
import chisel3.util.random.LFSR


abstract trait HasIO extends Module {
  val io: Bundle
}

private [nco] object NCOTableParams {
  val tableNameList = mutable.Set[String]()
}

case class NCOTableParams[T <: Data]
(
  phaseWidth: Int,
  phaseConv: UInt => T,
  protoTable: T,
  protoOut:   T,
  tableSize: Int,
  rasterizedMode: Boolean = false,
  nInterpolationTerms: Int = 0,
  tableName: String = "NCOTableLUT",
  ditherEnable: Boolean = false,
  syncROMEnable: Boolean = false,
  roundingMode: TrimType = RoundHalfUp
) {
  require(tableSize > 0, s"Table size must be greater than zero!")
  require(nInterpolationTerms >= 0, s"Taylor terms must be greater than or equal to zero!")
  //require(!NCOTableParams.tableNameList.contains(tableName), s"Name $tableName already used.")
  NCOTableParams.tableNameList.add(tableName)

  val addrBits = log2Ceil(tableSize)
}

object FixedNCOTableParams {
  def apply(tableSize: Int, tableWidth: Int, phaseWidth: Int = 8, rasterizedMode: Boolean = false, nInterpolationTerms: Int = 0, ditherEnable: Boolean = false, syncROMEnable: Boolean = false, roundingMode: TrimType = RoundHalfUp): NCOTableParams[FixedPoint] = {
    require(tableWidth >= 3)
    val phaseW = {
      if (rasterizedMode){
        (log2Ceil(tableSize)+2).toInt
      } else {
        phaseWidth
      }
    }
    NCOTableParams(
      phaseWidth = phaseW,
      phaseConv = {x: UInt => Cat(0.U, x).asTypeOf(FixedPoint.apply(width = phaseWidth.W, binaryPoint = phaseWidth.BP))},
      protoTable = FixedPoint((tableWidth).W, (tableWidth-2).BP),
      protoOut   = FixedPoint((tableWidth).W, (tableWidth-2).BP),
      tableSize  = tableSize,
      rasterizedMode = rasterizedMode,
      nInterpolationTerms = nInterpolationTerms,
      ditherEnable = ditherEnable,
      syncROMEnable = syncROMEnable,
      roundingMode = roundingMode
    )
  }
}

object DspRealNCOTableParams {
  def apply(tableSize: Int, rasterizedMode: Boolean = false): NCOTableParams[DspReal] = {
    val phaseW = (log2Ceil(tableSize)+2).toInt
    NCOTableParams(
      phaseWidth = phaseW,
      phaseConv = {x: UInt => (0.U).asReal()},
      protoTable = DspReal(),
      protoOut   = DspReal(),
      tableSize  = tableSize,
      rasterizedMode = rasterizedMode,
      nInterpolationTerms = 0,
      ditherEnable = false,
      syncROMEnable = false,
      roundingMode = RoundHalfUp
    )
  }
}


class NCOTableIO[T <: Data](params: NCOTableParams[T]) extends Bundle {
  val phase = Input(UInt(params.phaseWidth.W))
  val sinOut = Output(params.protoOut.cloneType)
  val cosOut = Output(params.protoOut.cloneType)
}


class NCOTableStandardMode[T <: Data : Ring : BinaryRepresentation : ConvertableTo](params: NCOTableParams[T]) extends Module with HasIO {

  val io = IO(new NCOTableIO(params))

  require(params.phaseWidth >= params.addrBits + 2,
    s"Input phase must have at least two more bits ($params.phaseWidth) than the address into the table ($params.addrBits).")

  require(isPow2(params.tableSize), s"Table Size must be a power of 2 when working in standard mode.")

  val totalWidth = params.phaseWidth


  val msbTop    = totalWidth
  val msbBot    = totalWidth - 2
  val addrTop   = msbBot
  val addrBot   = msbBot - log2Ceil(params.tableSize)
  val interpTop = addrBot
  val interpBot = 0

  val ditherWidth = interpTop + 1
  val lfsrVal = LFSR(16).asTypeOf(SInt(16.W))
  val ditherAdd = lfsrVal(15-1, 15 - ditherWidth).asTypeOf(SInt(ditherWidth.W))

  val phaseDither = Cat(0.U, io.phase).asSInt() + ditherAdd

  val msbs = {
    if (params.ditherEnable){
      phaseDither(msbTop-1, msbBot)
    } else {
      io.phase(msbTop-1, msbBot)
    }
  }

  val addr = {
    if (params.ditherEnable){
      phaseDither(addrTop-1, addrBot)
    } else {
      io.phase(addrTop-1, addrBot)
    }
  }

  val interp = {
    if (params.ditherEnable){
      phaseDither(0 max (interpTop-1), interpBot)
    } else {
      io.phase(0 max (interpTop-1), interpBot)
    }
  }

  val interp2 = Reg(UInt())
  interp2 := interp
  // interpret the lsbs as a Δθ
  val x = {
    if (params.syncROMEnable){
      params.phaseConv(interp2)
    } else {
      params.phaseConv(interp)
    }
  }

  val sinAddr = Wire(UInt())
  val cosAddr = Wire(UInt())

  val sinNegative = Reg(Bool())
  val cosNegative = Reg(Bool())

  val sinNegative2 = Wire(Bool())
  val cosNegative2 = Wire(Bool())

  sinAddr := 0.U
  cosAddr := 0.U
  sinNegative := false.B
  cosNegative := false.B
  sinNegative2 := false.B
  cosNegative2 := false.B

  val one = ConvertableTo[T].fromDouble(1.0)

  def addrReverse(addr: UInt): UInt = {
    0.U - addr
  }

  def zeroAddrReverse(addr: UInt): UInt = {
    (~addr).asUInt()
  }

  val sinIsOne = Reg(Bool())
  val cosIsOne = Reg(Bool())

  val sinIsOne2 = Wire(Bool())
  val cosIsOne2 = Wire(Bool())

  sinIsOne := false.B
  cosIsOne := false.B
  sinIsOne2 := false.B
  cosIsOne2 := false.B

  switch(msbs) {
    is("b00".U) {
      sinAddr := addr
      cosAddr := addrReverse(addr)
      sinNegative := false.B
      cosNegative := false.B
      sinNegative2 := false.B
      cosNegative2 := false.B
      cosIsOne := cosAddr === 0.U && addr === 0.U
      cosIsOne2 := cosAddr === 0.U && addr === 0.U
    }
    is("b01".U) {
      sinAddr := addrReverse(addr)
      cosAddr :=  addr
      sinNegative := false.B
      cosNegative := true.B
      sinNegative2 := false.B
      cosNegative2 := true.B
      sinIsOne := sinAddr === 0.U && addr === 0.U
      sinIsOne2 := sinAddr === 0.U && addr === 0.U
    }
    is("b10".U) {
      sinAddr := addr
      cosAddr := addrReverse(addr)
      sinNegative := true.B
      cosNegative := true.B
      sinNegative2 := true.B
      cosNegative2 := true.B
      cosIsOne := cosAddr === 0.U && addr === 0.U
      cosIsOne2 := cosAddr === 0.U && addr === 0.U
    }
    is("b11".U) {
      sinAddr := addrReverse(addr)
      cosAddr := addr
      sinNegative := true.B
      cosNegative := false.B
      sinNegative2 := true.B
      cosNegative2 := false.B
      sinIsOne := sinAddr === 0.U && addr === 0.U
      sinIsOne2 := sinAddr === 0.U && addr === 0.U
    }

  }

  val sinTableOut = Wire(params.protoTable)
  val cosTableOut = Wire(params.protoTable)

  if(params.syncROMEnable){
    val sinTable = DspContext.withTrimType(params.roundingMode) {
      (0 until params.tableSize).map { i =>
        val sinValue = sin(0.5 * Pi * i.toDouble / params.tableSize)
        val asT = ConvertableTo[T].fromDouble(sinValue, params.protoTable)
        asT.litValue()
      }
    }
    val table0 = Module(new SyncROM(params.tableName, sinTable))
    table0.io.addr1 := sinAddr
    table0.io.addr2 := cosAddr
    sinTableOut := Mux(sinIsOne, one, Cat(0.U, table0.io.data1).asTypeOf(params.protoTable))
    cosTableOut := Mux(cosIsOne, one, Cat(0.U, table0.io.data2).asTypeOf(params.protoTable))
  } else {
    def sinTable() = {
      DspContext.withTrimType(params.roundingMode) {
        val times = (0 until params.tableSize).map(i => (i.toDouble*0.5*Pi)/(params.tableSize))
        val inits = times.map(t => params.protoTable.fromDoubleWithFixedWidth(sin(t)))
        VecInit(inits)
      }
    }
    val table0 = sinTable()
    sinTableOut := Mux(sinIsOne2, one, table0(sinAddr).asTypeOf(params.protoTable))
    cosTableOut := Mux(cosIsOne2, one, table0(cosAddr).asTypeOf(params.protoTable))
  }


  val sinOut = {
    if(params.syncROMEnable){
      Mux(sinNegative, -sinTableOut, sinTableOut)
    } else {
      Mux(sinNegative2, -sinTableOut, sinTableOut)
    }
  }
  val cosOut = {
    if(params.syncROMEnable){
      Mux(cosNegative, -cosTableOut, cosTableOut)
    } else {
      Mux(cosNegative2, -cosTableOut, cosTableOut)
    }
  }

  // interpolation
  // coeffs go down as 1/n! * a_n
  // for sine, a_n is cos, -sin, -cos, sin, cos, ...
  // for cosine, a_n is the same sequence with the first term dropped

  val sinDerivs = Seq(cosOut, -sinOut, -cosOut, sinOut)
  val cosDerivs = Seq(-sinOut, -cosOut, sinOut, cosOut)

  def fact(i: Int): Double = {
    if (i <= 0) 1.0
    else (1 to i).map(BigInt(_)).product.toDouble
  }

  val n = params.protoOut match {
    case f: FixedPoint => f.binaryPoint.get
    case _ => 64
  }

  // compute terms of the taylor approximation
  val (sinTerms, cosTerms) = DspContext.withTrimType(params.roundingMode){ (Seq((sinOut, cosOut)) ++  (1 to params.nInterpolationTerms).map { i =>
    val coeff = ConvertableTo[T].fromDouble(math.pow(2.0 * Pi, i) / fact(i), params.protoOut)
    // coeff * x^i
    val coeff_x_xi = (coeff * TreeReduce(Seq.fill(i) {
      x
    }, (x: T, y: T) => (x * y).trimBinary(n))).trimBinary(n)
    val sinTerm = (coeff_x_xi * sinDerivs((i - 1) % 4)).trimBinary(n)
    val cosTerm = (coeff_x_xi * cosDerivs((i - 1) % 4)).trimBinary(n)
    (sinTerm, cosTerm)
  })}.unzip

  // add all the terms
  val sinInterp = DspContext.withTrimType(params.roundingMode){ TreeReduce(sinTerms, (x:T, y:T) => (x+y).trimBinary(n)) }
  val cosInterp = DspContext.withTrimType(params.roundingMode){ TreeReduce(cosTerms, (x:T, y:T) => (x+y).trimBinary(n)) }

  io.sinOut := Mux((params.phaseWidth).asUInt === (params.addrBits + 2).asUInt, sinOut, sinInterp)
  io.cosOut := Mux((params.phaseWidth).asUInt === (params.addrBits + 2).asUInt, cosOut, cosInterp)

}


class NCOTableRasterizedMode[T <: Data : Ring : BinaryRepresentation : ConvertableTo](params: NCOTableParams[T]) extends Module with HasIO {

  val io = IO(new NCOTableIO(params))

  val totalWidth = params.phaseWidth


  val msbTop    = totalWidth
  val msbBot    = totalWidth - 2
  val addrTop   = msbBot
  val addrBot   = msbBot - log2Ceil(params.tableSize)

  val addrR = Cat(0.U, (io.phase(msbTop-1, 0)).asUInt%(params.tableSize).U)
  val msbs = (((io.phase(msbTop-1, 0)).asUInt)/(params.tableSize).U)%4.U
  val addr = addrR

  val sinAddr = Wire(UInt())
  val cosAddr = Wire(UInt())

  val sinNegative = Reg(Bool())
  val cosNegative = Reg(Bool())

  val sinNegative2 = Wire(Bool())
  val cosNegative2 = Wire(Bool())

  sinAddr := 0.U
  cosAddr := 0.U
  sinNegative := false.B
  cosNegative := false.B
  sinNegative2 := false.B
  cosNegative2 := false.B

  val one = ConvertableTo[T].fromDouble(1.0)

  def addrReverse(addr: UInt): UInt = {
    params.tableSize - addr
  }

  def zeroAddrReverse(addr: UInt): UInt = {
    (~addr).asUInt()
  }

  val sinIsOne2 = Wire(Bool())
  val cosIsOne2 = Wire(Bool())
  val sinIsOne = Reg(Bool())
  val cosIsOne = Reg(Bool())

  sinIsOne := false.B
  cosIsOne := false.B
  sinIsOne2 := false.B
  cosIsOne2 := false.B

  switch(msbs) {
    is("b00".U) {
      sinAddr := addr
      cosAddr := addrReverse(addr)
      sinNegative := false.B
      cosNegative := false.B
      sinNegative2 := false.B
      cosNegative2 := false.B
      cosIsOne := cosAddr === (params.tableSize).U && addr === 0.U
      cosIsOne2 := cosAddr === (params.tableSize).U && addr === 0.U
    }
    is("b01".U) {
      sinAddr := addrReverse(addr)
      cosAddr :=  addr
      sinNegative := false.B
      cosNegative := true.B
      sinNegative2 := false.B
      cosNegative2 := true.B
      sinIsOne := sinAddr === (params.tableSize).U && addr === 0.U
      sinIsOne2 := sinAddr === (params.tableSize).U && addr === 0.U
    }
    is("b10".U) {
      sinAddr := addr
      cosAddr := addrReverse(addr)
      sinNegative := true.B
      cosNegative := true.B
      sinNegative2 := true.B
      cosNegative2 := true.B
      cosIsOne := cosAddr === (params.tableSize).U && addr === 0.U
      cosIsOne2 := cosAddr === (params.tableSize).U && addr === 0.U
    }
    is("b11".U) {
      sinAddr := addrReverse(addr)
      cosAddr := addr
      sinNegative := true.B
      cosNegative := false.B
      sinNegative2 := true.B
      cosNegative2 := false.B
      sinIsOne := sinAddr === (params.tableSize).U && addr === 0.U
      sinIsOne2 := sinAddr === (params.tableSize).U && addr === 0.U
    }
  }

  val sinTableOut = Wire(params.protoTable)
  val cosTableOut = Wire(params.protoTable)

  if(params.syncROMEnable){
    val sinTable = DspContext.withTrimType(params.roundingMode) {
      (0 until params.tableSize).map { i =>
        val sinValue = sin(0.5 * Pi * i.toDouble / params.tableSize)
        val asT = ConvertableTo[T].fromDouble(sinValue, params.protoTable)
        asT.litValue()
      }
    }
    val table0 = Module(new SyncROM(params.tableName, sinTable))
    table0.io.addr1 := sinAddr
    table0.io.addr2 := cosAddr
    sinTableOut := Mux(sinIsOne, one, Cat(0.U, table0.io.data1).asTypeOf(params.protoTable))
    cosTableOut := Mux(cosIsOne, one, Cat(0.U, table0.io.data2).asTypeOf(params.protoTable))
  } else {
    def sinTable() = {
      DspContext.withTrimType(params.roundingMode) {
        val times = (0 until params.tableSize).map(i => (i.toDouble*0.5*Pi)/(params.tableSize))
        val inits = times.map(t => params.protoTable.fromDoubleWithFixedWidth(sin(t)))
        VecInit(inits)
      }
    }
    val table0 = sinTable()
    sinTableOut := Mux(sinIsOne2, one, table0(sinAddr).asTypeOf(params.protoTable))
    cosTableOut := Mux(cosIsOne2, one, table0(cosAddr).asTypeOf(params.protoTable))
  }

  val sinOut = {
    if(params.syncROMEnable){
      Mux(sinNegative, -sinTableOut, sinTableOut)
    } else {
      Mux(sinNegative2, -sinTableOut, sinTableOut)
    }
  }
  val cosOut = {
    if(params.syncROMEnable){
      Mux(cosNegative, -cosTableOut, cosTableOut)
    } else {
      Mux(cosNegative2, -cosTableOut, cosTableOut)
    }
  }

  io.sinOut := sinOut
  io.cosOut := cosOut

}


class NCOTable[T <: Data : Ring : BinaryRepresentation : ConvertableTo](params: NCOTableParams[T]) extends Module with HasIO {

  val io = IO(new NCOTableIO(params))

  val tableNCO = {
    if (params.rasterizedMode){
      Module(new NCOTableRasterizedMode(params))
    } else {
      Module(new NCOTableStandardMode(params))
    }
  }

  tableNCO.io.phase := io.phase
  io.sinOut := tableNCO.io.sinOut
  io.cosOut := tableNCO.io.cosOut

}

