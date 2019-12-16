#!/usr/bin/env scala
import java.util.concurrent.LinkedBlockingQueue
import Integral.Implicits._
import scala.util.chaining._
import scala.jdk.CollectionConverters._
import java.util.concurrent.Executors

type ProgInt = BigInt
def ProgInt(value: String): ProgInt = BigInt(value)

implicit class RichInt(val me: Int) {
  def pow(other: Int) = math.pow(me, other).toInt
}

class IntcodeInterpreter(origProgram: Seq[ProgInt]) {
  var pc = 0  // Program counter
  var dp = 0  // Data pointer
  var program = origProgram.toBuffer

  sealed trait Opcode {
    def run(modes: Int): Unit
  }
  private case class UnitOpcode(args: Int, func: Seq[ProgInt] => Unit) extends Opcode {
    def run(modes: Int) = func(consumePtrs(args, modes))
  }
  private case class WritebackOpcode(args: Int, func: Seq[ProgInt] => ProgInt) extends Opcode {
    def run(modes: Int) = writePtr(func(consumePtrs(args, modes)), modes / 10.pow(args))
  }

  val opcodes = Map[Int, Opcode](
    1 -> WritebackOpcode(2, _.sum),
    2 -> WritebackOpcode(2, _.product),
    3 -> WritebackOpcode(0, { _ => io.StdIn.readLine("Need input: ").toInt}),
    4 -> UnitOpcode(1, { p => println(p(0)) }),
    5 -> UnitOpcode(2, { p => if (p(0) != 0) pc = p(1).toInt }),
    6 -> UnitOpcode(2, { p => if (p(0) == 0) pc = p(1).toInt }),
    7 -> WritebackOpcode(2, { p => if (p(0) < p(1)) 1 else 0 }),
    8 -> WritebackOpcode(2, { p => if (p(0) == p(1)) 1 else 0 }),
    9 -> UnitOpcode(1, { p => dp += p(0).toInt }),
    99 -> UnitOpcode(0, { _ => pc = -1 }),
  )

  def consume(count: Int): Seq[ProgInt] = {
    val res = (pc until (pc + count)).map(readFrom)
    pc += count
    res.toSeq
  }

  def consumePtrs(count: Int, modes: Int): Seq[ProgInt] = {
    def alterPtrs(ptrs: Seq[ProgInt], modes: Int): List[ProgInt] =
      ptrs match {
        case Seq() => Nil
        case ptr +: others => (modes % 10 match {
            case 0 => readFrom(ptr.toInt)
            case 1 => ptr
            case 2 => readFrom(dp + ptr.toInt)
          }) :: alterPtrs(others, modes / 10)
      }
    alterPtrs(consume(count), modes)
  }

  def writePtr(value: ProgInt, mode: Int): Unit = {
    val Seq(positionBig) = consume(1)
    val position = positionBig.toInt + (if (mode % 10 == 2) dp else 0)
    if (position >= program.size) {
      program ++= Iterator.fill(position - program.size + 1)(0)
    }
    program(position) = value
  }

  def readFrom(ptr: Int): ProgInt =
    program.applyOrElse(ptr, { _: Int => 0 })

  def step(): Unit = {
    val (modes, opcode) = consume(1)(0) /% 100
    opcodes.getOrElse(opcode.toInt, throw new Exception(s"Bad opcode $opcode"))
      .run(modes.toInt)
  }

  def run(): Unit = while (running) step()

  def running = pc >= 0
}

def main(): Unit = {
  val program = io.Source.fromFile("input").getLines
    .flatMap(_.split(",")).map(ProgInt(_)).toSeq

  new IntcodeInterpreter(program).run()
}

main()
