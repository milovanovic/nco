// SPDX-License-Identifier: Apache-2.0

package nco

import breeze.math.Complex
import chisel3.util.{log2Ceil, log2Floor}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.iotesters.PeekPokeTester
import chisel3.iotesters.Driver
// import dspblocks.PeekPokePackers
import dsptools._
//import dsptools.DspTester
import dsptools.numbers.DspReal
import scala.io.Source

trait NCOTableTester[T <: Data] { this: PeekPokeTester[NCOTable[T]] =>
  def pokePhase(in: BigInt): Unit
  def peekOut(): Complex
  def expectOut(cosVal: Double, sinVal: Double, tolLSB: Int): Unit

  def maxIdx = (1 << dut.io.phase.getWidth) - 1

}

class FixedNCOTableTester(c: NCOTable[FixedPoint]) extends DspTester(c) with NCOTableTester[FixedPoint] {
  def pokePhase(in: BigInt) = poke(c.io.phase, in)
  def peekOut() = Complex(peek(c.io.cosOut), peek(c.io.sinOut))
  def expectOut(cosVal: Double, sinVal: Double, tolLSB: Int): Unit = {}
}

class DspRealNCOTableTester(c: NCOTable[DspReal], override val maxIdx: Int = 16384) extends DspTester(c) with NCOTableTester[DspReal] {
  def pokePhase(in: BigInt) = poke(c.io.phase, in.toDouble / maxIdx.toDouble)
  def peekOut() = Complex(peek(c.io.cosOut), peek(c.io.sinOut))
  def expectOut(cosVal: Double, sinVal: Double, tolLSB: Int) = {
    fixTolLSBs.withValue(tolLSB) { expect(c.io.sinOut, sinVal) }
    fixTolLSBs.withValue(tolLSB) { expect(c.io.cosOut, cosVal) }
  }
}




class NCOStreamingPINCandPOFFTester(c: NCOStreamingPINCandPOFF[FixedPoint], tableSize: Int, rasterizedEnable: Boolean, syncROMEnable: Boolean, phaseAccEnable: Boolean, tolLSB : Int) extends DspTester(c) {
  
  def maxIdx = (1 << dut.io.freq.bits.getWidth)
  val factor = 1
  val max = {
    if (rasterizedEnable) {
      4*tableSize
    } else {
      maxIdx
    }
  }
  val phaseFactor = {
    if (rasterizedEnable){
      1
    } else {
      maxIdx/(4*tableSize)
    }
  }
  
  updatableDspVerbose.withValue(false) {

    var phaseOffset = 0//1
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.poff.valid, 0)
    step(2)
    var idx = if (syncROMEnable) 1 else 0

    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    poke(dut.io.poff.valid, 1)
    poke(dut.io.poff.bits, phaseOffset)


    poke(dut.io.freq.bits, idx)
    if (syncROMEnable && !phaseAccEnable) idx +=1
    step(1)

    while (idx < max) {
      val sinCalc = {
        if (!syncROMEnable){
          if (phaseAccEnable){
            scala.math.sin(0.5 * scala.math.Pi *(idx+phaseOffset).toDouble/(phaseFactor*tableSize).toDouble)
          } else {
            scala.math.sin(0.5 * scala.math.Pi *(idx).toDouble/(phaseFactor*tableSize).toDouble)
          }
        } else {
          if (phaseAccEnable){
            val x = if(idx == 0) idx else (idx-1)
            scala.math.sin(0.5 * scala.math.Pi *x.toDouble/(phaseFactor*tableSize).toDouble)
          } else {
            val x = if(idx == 1) (idx-1) else (idx-2)
            scala.math.sin(0.5 * scala.math.Pi *x.toDouble/(phaseFactor*tableSize).toDouble)
          }
        }
      }
      val cosCalc = {
        if (!syncROMEnable){
          if (phaseAccEnable){
            scala.math.cos(0.5 * scala.math.Pi *(idx+phaseOffset).toDouble/(phaseFactor*tableSize).toDouble)
          } else {
            scala.math.cos(0.5 * scala.math.Pi *(idx).toDouble/(phaseFactor*tableSize).toDouble)
          }
        } else {
          if (phaseAccEnable){
            val x = if(idx == 0) idx else (idx-1)
            scala.math.cos(0.5 * scala.math.Pi *x.toDouble/(phaseFactor*tableSize).toDouble)
          } else {
            val x = if(idx == 1) (idx-1) else (idx-2)
            scala.math.cos(0.5 * scala.math.Pi *x.toDouble/(phaseFactor*tableSize).toDouble)
          }
        }
      }

      if (peek(c.io.out.valid)) {
        fixTolLSBs.withValue(tolLSB) { expect(c.io.out.bits.imag, sinCalc) }
        fixTolLSBs.withValue(tolLSB) { expect(c.io.out.bits.real, cosCalc) }
        idx += 1
      }
      if (phaseAccEnable) {
        poke(dut.io.freq.bits, factor)
      } else {
        if (idx < max) {
          poke(dut.io.freq.bits, idx)
        }
      }
      step(1)
    }


    /*poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.poff.valid, 0)
    step(5)
    poke(dut.io.freq.bits, 1)
    poke(dut.io.freq.valid, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(10)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)*/

    /*step(2)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    step(2)
    poke(dut.io.freq.valid, 1)
    step(10)*/

    /*poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(10)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(5)
    poke(dut.io.out.ready, 1)
    step(10)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.out.ready, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.freq.valid, 1)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(6)

    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.out.ready, 1)
    step(2)
    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.out.ready, 1)
    step(2)
    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.out.ready, 1)
    step(2)
    poke(dut.io.out.ready, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(2)
    poke(dut.io.freq.valid, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    poke(dut.io.out.ready, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(2)
    poke(dut.io.freq.valid, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(1)
    poke(dut.io.freq.valid, 1)
    step(5)
    poke(dut.io.freq.valid, 0)
    step(2)
    poke(dut.io.freq.valid, 1)
    step(2)
    poke(dut.io.freq.valid, 0)
    step(2)
    poke(dut.io.freq.valid, 1)
    step(6)

    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(1)
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    step(6)
    poke(dut.io.freq.valid, 0)
    step(4)
    poke(dut.io.freq.valid, 1)



    step(100)*/
  }
  
}


class NCOStreamingPINCandPOFFTesterDspReal(c: NCOStreamingPINCandPOFF[DspReal], tableSize: Int, rasterizedEnable: Boolean, phaseAccEnable: Boolean, tolLSB : Int) extends DspTester(c) {

  def maxIdx = (1 << dut.io.freq.bits.getWidth)
  val factor = 1
  val max = {
    if (rasterizedEnable) {
      4*tableSize
    } else {
      maxIdx
    }
  }
  val phaseFactor = {
    if (rasterizedEnable) {
      1
    } else {
      maxIdx/(4*tableSize)
    }
  }

  updatableDspVerbose.withValue(false) {
  
    var phaseOffset = 1
    poke(dut.io.out.ready, 0)
    poke(dut.io.freq.valid, 0)
    poke(dut.io.poff.valid, 0)
    step(2)

    var idx = 0
    poke(dut.io.freq.bits, idx)
    if(phaseAccEnable) {
      poke(dut.io.poff.bits, phaseOffset)
    }
    poke(dut.io.out.ready, 1)
    poke(dut.io.freq.valid, 1)
    poke(dut.io.poff.valid, 1)

    step(1)

    while (idx < max) {
      step(1)
      val sinCalc = {
        if(phaseAccEnable) {
          scala.math.sin(0.5 * scala.math.Pi *(idx+phaseOffset).toDouble/(phaseFactor*tableSize).toDouble)
        } else {
          scala.math.sin(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
        }
      }
      val cosCalc = {
        if(phaseAccEnable){
          scala.math.cos(0.5 * scala.math.Pi *(idx+phaseOffset).toDouble/(phaseFactor*tableSize).toDouble)
        } else {
          scala.math.cos(0.5 * scala.math.Pi *idx.toDouble/(phaseFactor*tableSize).toDouble)
        }
      }

      if (peek(c.io.out.valid)) {
        fixTolLSBs.withValue(tolLSB) { expect(c.io.out.bits.imag, sinCalc) }
        fixTolLSBs.withValue(tolLSB) { expect(c.io.out.bits.real, cosCalc) }
      }
      idx += 1
      if (phaseAccEnable) {
        poke(dut.io.freq.bits, factor)
      } else {
        if (idx < max) {
          poke(dut.io.freq.bits, idx)
        }
      }
    }
  }
    
}
