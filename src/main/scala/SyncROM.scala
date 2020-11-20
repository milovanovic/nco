package dsptools

import chisel3._
import chisel3.util.HasBlackBoxInline
import firrtl.ir.Type

import scala.collection.mutable


class SyncROM(val blackboxName: String, val table: Seq[BigInt], val widthOverride: Option[Int] = None)
extends Module {
  val dataWidth = SyncROMBlackBox.dataWidth(table, widthOverride)

  val addrWidth = SyncROMBlackBox.addrWidth(table)

  val io = IO(new SyncROMIO(addrWidth=addrWidth, dataWidth=dataWidth))

  val rom = Module(new SyncROMBlackBox(blackboxName, table, widthOverride))

  rom.io.clock  := clock
  rom.io.addr1  := io.addr1
  rom.io.addr2  := io.addr2
  io.data1      := rom.io.data1
  io.data2      := rom.io.data2
}

trait HasBlackBoxClock {
  val clock = Input(Clock())
}

class SyncROMIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val addr1  = Input(UInt(addrWidth.W))
  val addr2  = Input(UInt(addrWidth.W))
  val data1  = Output(UInt(dataWidth.W))
  val data2  = Output(UInt(dataWidth.W))
}

class SyncROMBlackBox(blackboxName: String, table: Seq[BigInt], widthOverride: Option[Int] = None)
extends BlackBox with HasBlackBoxInline {
  val dataWidth = SyncROMBlackBox.dataWidth(table, widthOverride)

  val addrWidth = SyncROMBlackBox.addrWidth(table)

  val io = IO(new SyncROMIO(addrWidth=addrWidth, dataWidth = dataWidth) with HasBlackBoxClock)

  override def desiredName: String = blackboxName

  def tableEntry2CaseStr1(value: BigInt, addr1: BigInt): String = {
    s"$addrWidth'b${addr1.toString(2)}: data1 <= $dataWidth'h${value.toString(16)};"
  }
  def tableEntry2CaseStr2(value: BigInt, addr2: BigInt): String = {
    s"$addrWidth'b${addr2.toString(2)}: data2 <= $dataWidth'h${value.toString(16)};"
  }
  val tableStrings1 = table.zipWithIndex.map { case (t, i) => tableEntry2CaseStr1(t, BigInt(i))}
  val tableString1  = tableStrings1.foldLeft("\n") { case (str, entry) => str + "      " + entry + "\n"}
  val tableStrings2 = table.zipWithIndex.map { case (t, i) => tableEntry2CaseStr2(t, BigInt(i))}
  val tableString2  = tableStrings2.foldLeft("\n") { case (str, entry) => str + "      " + entry + "\n"}

  val verilog =
    s"""
    |module $name(
    |  input                                    clock,
    |  input      [${(addrWidth - 1).max(0)}:0] addr1,
    |  input      [${(addrWidth - 1).max(0)}:0] addr2,
    |  output reg [${(dataWidth - 1).max(0)}:0] data1,
    |  output reg [${(dataWidth - 1).max(0)}:0] data2
    |);
    |  always @(posedge clock) begin
    |    case (addr1)$tableString1
    |      default: data1 <= $dataWidth'h0;
    |    endcase
    |    case (addr2)$tableString2
    |      default: data2 <= $dataWidth'h0;
    |    endcase
    |  end
    |endmodule
    """.stripMargin

  setInline(s"$name.v", verilog)

  SyncROMBlackBox.interpreterMap.update(name, (table, dataWidth))
}

object SyncROMBlackBox {
  def addrWidth(table: Seq[BigInt]): Int = {
    BigInt(table.length - 1).bitLength
  }
  def dataWidth(table: Seq[BigInt], widthOverride: Option[Int]): Int = {
    val w = widthOverride.getOrElse(table.map{_.bitLength}.max)
    require(w >= table.map{_.bitLength}.max, "width too small for table")
    w
  }
  private [dsptools] val interpreterMap = mutable.Map[String, (Seq[BigInt], Int)]()
}

// implementation for firrtl interpreter
class SyncROMBlackBoxImplementation(val name: String, val table: Seq[BigInt], dataWidth: Int, default: BigInt = 0)
extends firrtl_interpreter.BlackBoxImplementation {
  import firrtl_interpreter._

  var lastCycleAddr: BigInt    = BigInt(0)
  var currentCycleAddr: BigInt = BigInt(0)
  override def cycle(): Unit = {
    println("cycle got called")
    lastCycleAddr = currentCycleAddr
  }

  override def execute(inputValues: Seq[Concrete], tpe: Type, outputName: String): Concrete = {
    require(outputName == "data", s"Only output should be data, got $outputName")
    currentCycleAddr = inputValues.head.value
    println(s"execute got called on $outputName for addr $currentCycleAddr")
    val tableValue = if (lastCycleAddr.toInt < table.length) {
      table(lastCycleAddr.toInt)
    } else {
      default
    }
    ConcreteUInt(tableValue, dataWidth, inputValues.head.poisoned)
  }

  override def outputDependencies(outputName: String) = Seq("addr")
}

class SyncROMBlackBoxFactory extends firrtl_interpreter.BlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String) =
    SyncROMBlackBox.interpreterMap.get(blackBoxName).map {
      case (table, dataWidth) => new SyncROMBlackBoxImplementation(instanceName, table, dataWidth)
    }
}
