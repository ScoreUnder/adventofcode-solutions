#!/usr/bin/env scala
import java.util.concurrent.LinkedBlockingQueue
import Integral.Implicits._
import scala.util.chaining._
import scala.jdk.CollectionConverters._
import java.util.concurrent.Executors

class IntcodeInterpreter(origProgram: Seq[Int]) {
  var pc = 0
  var program = origProgram.toBuffer
  val input = new LinkedBlockingQueue[Int]
  val output = new LinkedBlockingQueue[Int]

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
    3 -> WritebackOpcode(0, { _ => input.take() }),
    4 -> UnitOpcode(1, { v => output.add(v(0)) }),
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

  def run(): Unit = while (running) step()

  def running = pc >= 0
}

def main(): Unit = {
  val program = io.Source.fromFile("input").getLines
    .flatMap(_.split(",")).map(_.toInt).toSeq

  def part1 = {
    val inputs = (0 to 4).permutations
    val results = for (input <- inputs)
      yield input -> input.foldLeft(0) { (acc, inp) =>
        new IntcodeInterpreter(program).tap { interp =>
          interp.input.addAll(List(inp, acc).asJava)
          interp.run()
        }.output.take()
      }
    results.maxBy(_._2)
  }

  def part2 = {
    val inputs = (5 to 9).permutations
    val threadPool = Executors.newCachedThreadPool()
    val results = for (input <- inputs)
      yield {
        val machines = (for (digit <- input)
          yield new IntcodeInterpreter(program).tap { interp =>
            interp.input.add(digit)
          }).toSeq
        val machinesRunning = (for (machine <- machines)
          yield threadPool.submit({ () => machine.run() }: Runnable)).toSeq
        val pipes = (for ((out, in) <- machines zip (machines.tail :+ machines.head))
          yield threadPool.submit({ () => while (true) { in.input.add(out.output.take()) } }: Runnable)).toSeq
        machines(0).input.add(0)
        while (machines(0).running) {
          // slightly-less-busy wait
          machines(0).input.add(machines(0).input.take())
        }
        val result = machines(0).input.take()
        machinesRunning.foreach { _.cancel(true) }
        pipes.foreach { _.cancel(true) }
        input -> result
      }
    val result = results.maxBy(_._2)
    threadPool.shutdownNow()
    result
  }

  println(part1)
  println(part2)
}

main()
