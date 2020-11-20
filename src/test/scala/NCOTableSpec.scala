package nco

import breeze.math.Complex
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source

class NCOTableSpec extends FlatSpec with Matchers {
  def dut[T <: Data : Ring : BinaryRepresentation : ConvertableTo](params: NCOTableParams[T]): () => NCOTable[T] = () => {
    NCOTableParams.tableNameList.clear()
    new NCOTable(params)
  }

  behavior of "NCO Table"


  it should """run the tester 1: Comparing with python model results, NCO with table size of 2048,
    table width of 22 bits, phase width of 13 bits, working in standard mode with Taylor series terms of 2,
    without phase dithering, with syncROM""" in {
    val tableSize = 2048
    val phaseWidth = 13
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM,
    val fixedParams = FixedNCOTableParams(2048, 22, 13, false, 2, false, false)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = scala.math.pow(2.0, (phaseWidth)).toInt
        val file: String = "2048_22_13_false_2_false_false.txt"
        val source = Source.fromFile(file)
        val columnsTogether = source.getLines.map { line =>
          val nums = line.split(" ")
          (nums.head, nums.last)
        }.toList
        val sinVals = columnsTogether.map(_._1.toDouble)
        val cosVals = columnsTogether.map(_._2.toDouble)
        var idx = 0
        val factor = 1

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val thresh = scala.math.pow(2.0, -30)
          val sinErr = (sine.toDouble - sinVals(idx).toDouble).abs
          val cosErr = (cosine.toDouble - cosVals(idx).toDouble).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 2: Comparing with python model results, NCO with table size of 256,
    table width of 14 bits, phase width of 11 bits, working in standard mode with Taylor series terms of 4,
    without phase dithering, without syncROM""" in {
    val tableSize = 256
    val phaseWidth = 11
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(256, 14, 11, false, 4, false, false)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = scala.math.pow(2.0, phaseWidth).toInt
        val file: String = "256_14_11_false_4_false_false.txt"
        val source = Source.fromFile(file)
        val columnsTogether = source.getLines.map { line =>
          val nums = line.split(" ")
          (nums.head, nums.last)
        }.toList
        val sinVals = columnsTogether.map(_._1.toDouble)
        val cosVals = columnsTogether.map(_._2.toDouble)
        var idx = 0
        val factor = 1

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val thresh = scala.math.pow(2.0, -30)
          val sinErr = (sine.toDouble - sinVals(idx).toDouble).abs
          val cosErr = (cosine.toDouble - cosVals(idx).toDouble).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 3: Comparing with python model results, NCO with table size of 400,
    table width of 20 bits, phase width of 11 bits, working in rasterized mode without syncROM""" in {
    val tableSize = 400
    val phaseWidth = 11
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(400, 20, 11, true, 0, false, true)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = 4*tableSize
        val file: String = "400_20_11_true_0_false_false.txt"
        val source = Source.fromFile(file)
        val columnsTogether = source.getLines.map { line =>
          val nums = line.split(" ")
          (nums.head, nums.last)
        }.toList
        val sinVals = columnsTogether.map(_._1.toDouble)
        val cosVals = columnsTogether.map(_._2.toDouble)
        var idx = 0
        val factor = 1

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val thresh = scala.math.pow(2.0, -30)
          val sinErr = (sine.toDouble - sinVals(idx).toDouble).abs
          val cosErr = (cosine.toDouble - cosVals(idx).toDouble).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 4: Comparing with results calculated with scala.math functions, NCO with table size of 512,
    table width of 20 bits, phase width of 13 bits, working in standard mode with Taylor series terms of 3,
    without phase dithering, with syncROM""" in {
    val tableSize = 512
    val phaseWidth = 13
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(512, 20, 13, false, 3, false, true)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = scala.math.pow(2.0, phaseWidth).toInt
        var idx = 0
        val factor = 1
        val phaseFactor = max/(4*tableSize)

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val sinCalc = scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val cosCalc = scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val thresh = 0.00001
          val sinErr = (sine - sinCalc).abs
          val cosErr = (cosine - cosCalc).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 5: Comparing with results calculated with scala.math functions, NCO with table size of 128,
    table width of 8 bits, phase width of 10 bits, working in standard mode without Taylor series corrections,
    without phase dithering, with syncROM""" in {
    val tableSize = 128
    val phaseWidth = 10
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(128, 8, 10, false, 0, false, true)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = scala.math.pow(2.0, phaseWidth).toInt//4*tableSize
        var idx = 0
        val factor = 1
        val phaseFactor = max/(4*tableSize)

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val sinCalc = scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val cosCalc = scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val thresh = 0.01
          val sinErr = (sine - sinCalc).abs
          val cosErr = (cosine - cosCalc).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 6: Comparing with results calculated with scala.math functions, NCO with table size of 133,
    table width of 14 bits, phase width of 10 bits, working in rasterized mode, without syncROM""" in {
    val tableSize = 133
    val phaseWidth = 10
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(133, 14, 11, true, 0, false, false)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = 4*tableSize
        var idx = 0
        val factor = 1
        val phaseFactor = max/(4*tableSize)

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val sinCalc = scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val cosCalc = scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
          val thresh = 0.01
          val sinErr = (sine - sinCalc).abs
          val cosErr = (cosine - cosCalc).abs
          sinErr should be < thresh
          cosErr should be < thresh
          idx += 1 * factor
        }
      }
    }
  }

  it should """run the tester 7: Comparing with results calculated with scala.math functions, NCO with table size of 256,
    table width of 12 bits, phase width of 13 bits, working in standard mode without Taylor series correction,
    with phase dithering, with syncROM""" in {
    val tableSize = 256
    val phaseWidth = 13
    //tableSize, tableWidth, phaseWidth, rasterized, taylorTerms, dither, syncROM
    val fixedParams = FixedNCOTableParams(256, 12, 13, false, 0, true, true)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(fixedParams)) {
      c => new FixedNCOTableTester(c) {
        val max = scala.math.pow(2.0, phaseWidth).toInt
        var idx = 0
        val factor = 1
        poke(c.io.phase, factor)
        step(1)
        while (idx <= (max - factor)) {
          //poke(c.io.phase, idx)
          //step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          idx += 1 * factor
          poke(c.io.phase, factor)
          step(1)
        }
      }
    }
  }

  it should "run the tester 8: DspReal tester, NCO with table size of 64, working in standard mode" in {
    val tableSize = 64
    //tableSize, rasterized
    val DspRealParams = DspRealNCOTableParams(tableSize, false)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(DspRealParams)) {
      c => new DspRealNCOTableTester(c) {
        val max = 4*tableSize
        var idx = 0
        val factor = 1

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val sinCalc = scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/tableSize.toDouble)
          val cosCalc = scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/tableSize.toDouble)
          this.expectOut(cosCalc, sinCalc, 0)
          idx += 1 * factor
        }
      }
    } should be (true)
  }


  it should "run the tester 9: DspReal tester, NCO with table size of 287, working in rasterized mode" in {
    val tableSize = 287
    //tableSize, rasterized
    val DspRealParams = DspRealNCOTableParams(tableSize, true)
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), dut(DspRealParams)) {
      c => new DspRealNCOTableTester(c) {
        val max = 4*tableSize
        var idx = 0
        val factor = 1

        while (idx <= (max - factor)) {
          poke(c.io.phase, idx)
          step(1)
          val sine = peek(c.io.sinOut)
          val cosine = peek(c.io.cosOut)
          val sinCalc = scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/tableSize.toDouble)
          val cosCalc = scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/tableSize.toDouble)
          this.expectOut(cosCalc, sinCalc, 0)
          idx += 1 * factor
        }
      }
    } should be (true)
  }

}
