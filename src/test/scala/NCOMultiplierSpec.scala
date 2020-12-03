package nco


import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.math.{Pi, pow, sqrt}
import breeze.plot._
import java.io._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}



class NCOMultiplierTester(dut: AXI4NCOLazyModuleBlock[FixedPoint] with AXI4Block,
  csrAddress: AddressSet,
  beatBytes : Int
) extends PeekPokeTester(dut.module) with AXI4MasterModel {
  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  
  
  memWriteWord(csrAddress.base, 1) // enable multiplying
  step(1)
  memWriteWord(csrAddress.base + beatBytes, 0x00002000) // multiplying factor
  step(1)
  memWriteWord(csrAddress.base + 2 * beatBytes, 1) // freq reg
  step(1)
  
/*  
  poke(dut.out.ready, 1)
  step(50)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(20)
  poke(dut.out.ready, 0)
  step(10)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  poke(dut.out.ready, 0)
  step(1)
  poke(dut.out.ready, 1)
  step(10)*/
  val returnVal = new Array[BigInt](1024)
  val returnVal1 = new Array[Int](1024)
  val real = new Array[Double](1024)
  val imag = new Array[Double](1024)
  var idx = 0
  
  step(1)
  poke(dut.out.ready, 1)
  step(1)
  var ii = 0
  while ((ii < 1300) && (idx < 512)) {
    if(peek(dut.out.valid) > 0) {
      returnVal(idx) = peek(dut.out.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }
  poke(dut.out.ready, 0)
  step(1)
  memWriteWord(csrAddress.base + beatBytes, 0x00001000) // multiplying factor
  step(1)
  poke(dut.out.ready, 1)
  while ((ii < 1300) && (idx < 1024)) {
    if(peek(dut.out.valid) > 0) {
      returnVal(idx) = peek(dut.out.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }

  
  val f1 = Figure("NCO Multiplier Output")
  val p1 = f1.subplot(1,1,0)
  p1.legend_= (true)
  val xaxis1 = (0 until real.length).map(e => e.toDouble).toSeq.toArray
  p1.setXAxisIntegerTickUnits()
  p1 += plot(xaxis1, real.toArray, name = "Real output value")
  p1 += plot(xaxis1, imag.toArray, name = "Imag output value")
  p1.ylim(real.min, real.max)
  //p1.ylim(returnVal1.min, returnVal1.max)
  p1.xlabel = "Time"
  p1.ylabel = "Signal values"
  f1.saveas(s"test_run_dir/nco_multiplier.pdf")

}


class NCOMultiplierSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  
  // here just define parameters
  val paramsNCO = FixedNCOParams(
    tableSize = 256,
    tableWidth = 16,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Config,
    poffType = Fixed,
    useMultiplier = true
  )
  val beatBytes = 4
  
  it should "Test NCO Multiplier" in {
    val lazyDut = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = 4)  with AXI4Block {
      override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new NCOMultiplierTester(lazyDut,  AddressSet(0x000000, 0xFF), 4)
    } should be (true)
  }
}
