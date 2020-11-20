package nco

import breeze.math.Complex
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import dsptools.RoundHalfUp

class NCOStreamingPINCandPOFFSpec extends FlatSpec with Matchers {
  def dut[T <: Data : Real : BinaryRepresentation](params: NCOParams[T]): () => NCOStreamingPINCandPOFF[T] = () => {
    NCOTableParams.tableNameList.clear()
    new NCOStreamingPINCandPOFF(params)
  }

  behavior of "NCO"

  for(phaseAcc <- Seq(true, false)) {
    for(syncROM <- Seq(true, false)) {
      it should f"""run the tester: NCO with table size of 128, table width of 16 bits,
        phase width of 9 bits, working in standard mode without Taylor series correction,
        without phase dithering, syncROM = $syncROM, phase accumulator = $phaseAcc, streaming phase increment and offset""" in {
        //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM, phaseAccEnable
        val fixedParams = FixedNCOParams(128, 16, 9, false, 0, false, syncROM, phaseAcc, RoundHalfUp)
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
          //nco, tableSize, rasterized, syncROM, phaseAcc, tolLSB
          c => new NCOStreamingPINCandPOFFTester(c, 128, false, syncROM, phaseAcc, 5)
        } should be (true)
      }

      it should f"""run the tester: NCO with table size of 500, table width of 12 bits,
        phase width of 11 bits, working in rasterized mode without Taylor series correction,
        without phase dithering, syncROM = $syncROM, phase accumulator = $phaseAcc, streaming phase increment and offset""" in {
        //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM, phaseAccEnable
        val fixedParams = FixedNCOParams(500, 12, 11, true, 0, false, syncROM, phaseAcc, RoundHalfUp)
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
          //nco, tableSize, rasterized, syncROM, phaseAcc, tolLSB
          c => new NCOStreamingPINCandPOFFTester(c, 500, true, syncROM, phaseAcc, 5)
        } should be (true)
      }

      it should f"""run the tester: NCO with table size of 512, table width of 18 bits,
        phase width of 13 bits, working in standard mode with Taylor series of 4,
        without phase dithering, syncROM = $syncROM, phase accumulator = $phaseAcc, streaming phase increment and offset""" in {
        //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM, phaseAccEnable
        val fixedParams = FixedNCOParams(512, 18, 13, false, 4, false, syncROM, phaseAcc, RoundHalfUp)
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
          //nco, tableSize, rasterized, syncROM, phaseAcc, tolLSB
          c => new NCOStreamingPINCandPOFFTester(c, 512, false, syncROM, phaseAcc, 4)
        } should be (true)
      }

    }


    it should f"run the tester: DspReal tester, NCO with table size of 89, working in rasterized mode, phase accumulator = $phaseAcc, streaming phase increment and offset" in {
      val DspRealParams = DspRealNCOParams(89, true, phaseAcc)
      chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(DspRealParams)) {
        //nco, tableSize, rasterized, phaseAcc, tolLSB
        c => new NCOStreamingPINCandPOFFTesterDspReal(c, 89, true, phaseAcc, 8)
      } should be (true)
    }

    it should f"run the tester: DspReal tester, NCO with table size of 256, working in standard mode, phase accumulator = $phaseAcc, streaming phase increment and offset" in {
      val DspRealParams = DspRealNCOParams(256, false, phaseAcc)
      chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(DspRealParams)) {
        //nco, tableSize, rasterized, phaseAcc, tolLSB
        c => new NCOStreamingPINCandPOFFTesterDspReal(c, 256, false, phaseAcc, 8)
      } should be (true)
    }

  }
  
  
  /*val syncROM = true
  val phaseAcc = false
  it should f"""run the tester: NCO with table size of 128, table width of 16 bits,
    phase width of 9 bits, working in standard mode without Taylor series correction,
    without phase dithering, syncROM = $syncROM, phase accumulator = $phaseAcc, streaming phase increment and offset""" in {
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM, phaseAccEnable
    val fixedParams = FixedNCOParams(128, 16, 9, false, 0, false, syncROM, phaseAcc, RoundHalfUp)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      //nco, tableSize, rasterized, syncROM, phaseAcc, tolLSB
      c => new NCOStreamingPINCandPOFFTester(c, 128, false, syncROM, phaseAcc, 5)
    } should be (true)
  }*/


}
