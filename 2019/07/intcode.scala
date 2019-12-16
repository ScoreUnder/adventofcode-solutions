#!/usr/bin/env scala
import collection.mutable
import Integral.Implicits._
import scala.util.chaining._

class IntcodeInterpreter(origProgram: Seq[Int]) {
  var pc = 0
  var program = origProgram.toBuffer
  val input = mutable.Queue.empty[Int]
  val output = mutable.Queue.empty[Int]

  sealed trait Opcode {
    def run(modes: Int): Unit
  }
  private case class UnitOpcode(args: Int, func: Seq[Int] => Unit) extends Opcode {
    def run(modes: Int) = func(consumePtrs(args, modes))
  }
  private case class WritebackOpcode(args: Int, func: Seq[Int] => Int) extends Opcode {
    def run(modes: Int) = writePtr(func(consumePtrs(args, modes)))
  }

  val opcodes = Map[Int, Opcode](
    1 -> WritebackOpcode(2, _.sum),
    2 -> WritebackOpcode(2, _.product),
    3 -> WritebackOpcode(0, { _ => input.dequeue() }),
    4 -> UnitOpcode(1, { v => output ++= v }),
    5 -> UnitOpcode(2, { p => if (p(0) != 0) pc = p(1) }),
    6 -> UnitOpcode(2, { p => if (p(0) == 0) pc = p(1) }),
    7 -> WritebackOpcode(2, { p => if (p(0) < p(1)) 1 else 0 }),
    8 -> WritebackOpcode(2, { p => if (p(0) == p(1)) 1 else 0 }),
    99 -> UnitOpcode(0, { _ => pc = -1 }),
  )

  def consume(count: Int): Seq[Int] = {
    val res = program.slice(pc, pc + count)
    pc += count
    res.toSeq
  }

  def consumePtrs(count: Int, modes: Int): Seq[Int] = {
    def alterPtrs(ptrs: Seq[Int], modes: Int): List[Int] =
      if (ptrs.isEmpty) Nil
      else if (modes % 10 == 1) ptrs.head :: alterPtrs(ptrs.tail, modes / 10)
      else program(ptrs.head) :: alterPtrs(ptrs.tail, modes / 10)
    alterPtrs(consume(count), modes)
  }

  def writePtr(value: Int): Unit = program(consume(1)(0)) = value

  def step(): Unit = {
    val (modes, opcode) = consume(1)(0) /% 100
    opcodes.getOrElse(opcode, throw new Exception(s"Bad opcode $opcode"))
      .run(modes)
  }

  def run(): Unit = while (pc >= 0) step()
}

def main(): Unit = {
  val program = io.Source.fromFile("input").getLines
    .flatMap(_.split(",")).map(_.toInt).toSeq
  val inputs = (0 to 4).permutations
  val results = for (input <- inputs)
    yield input -> input.foldLeft(0) { (acc, inp) =>
      new IntcodeInterpreter(program).tap { interp =>
        interp.input ++= inp :: acc :: Nil
        interp.run()
      }.output.dequeue()
    }
  println(results.maxBy(_._2))
}

main()
