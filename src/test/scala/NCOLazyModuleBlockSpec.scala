// SPDX-License-Identifier: Apache-2.0

package nco


import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

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



class NCOLazyModuleBlockTester(dut: AXI4NCOLazyModuleBlock[FixedPoint] with AXI4Block,
  csrAddress: AddressSet,
  beatBytes : Int
) extends PeekPokeTester(dut.module) with AXI4MasterModel {
  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  
  /*
  // conf, conf
  memWriteWord(csrAddress.base, 1)
  step(1)
  memWriteWord(csrAddress.base + beatBytes, 20)
  step(2)
  poke(dut.out.ready, 1)
  step(500)
  memWriteWord(csrAddress.base, 5)
  step(200)
  */
  /*
  // fixed, conf
  memWriteWord(csrAddress.base, 1)
  step(1)
  memWriteWord(csrAddress.base + beatBytes, 20)
  step(2)
  poke(dut.out.ready, 1)
  step(600)
  */
  /*
  // fixed, fixed
  memWriteWord(csrAddress.base, 1)
  step(1)
  poke(dut.out.ready, 1)
  step(600)
  */
  
  // conf, fixed
  memWriteWord(csrAddress.base, 1)
  step(1)
  poke(dut.out.ready, 1)
  step(500)
  memWriteWord(csrAddress.base, 5)
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
  step(2)
  poke(dut.out.ready, 0)
  step(2)
  
  /*poke(dut.freq.get.in(0)._1.bits.data, 1)
  step(1)
  poke(dut.freq.get.in(0)._1.bits.last, 0)
  step(1)
  poke(dut.freq.get.in(0)._1.valid, 1)
  step(1)
  poke(dut.out.ready, 1)
  step(20)
  var i = 0
  while(i<20){
    poke(dut.out.ready, 0)
    step(1)
    poke(dut.out.ready, 1)
    step(1)
    i+=1
  }*/
  
  

}


class NCOLazyModuleBlockSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  
  // here just define parameters
  val paramsNCO = FixedNCOParams(
    tableSize = 256,
    tableWidth = 16,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = true,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Config,
    poffType = Fixed
  )
  val beatBytes = 4
  
  it should "Test NCO LazyModule Block" in {
    val lazyDut = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO, AddressSet(0x000000, 0xFF), beatBytes = 4)  with AXI4Block {
      override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new NCOLazyModuleBlockTester(lazyDut,  AddressSet(0x000000, 0xFF), 4)
    } should be (true)
  }
}
