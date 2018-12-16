val TESTCASE_REGEX = """^(\w+):\s+\[([^]]*)\]$""".r
val OPCODE_REGEX = """^(\d+(?:\s+\d+){3})$""".r

type Regs = Vector[Int]
type Opcode = Vector[Int]

case class MyOp(code: Int)
case class TestCase(inRegs: Regs, outRegs: Regs, opcode: Opcode)

def operation(regs: Regs, op: MyOp, opcode: Opcode): Regs = {
  val Seq(_, a, b, c) = opcode
  regs.updated(c, op.code match {
    case 0 => regs(a) + regs(b)
    case 1 => regs(a) + b

    case 2 => regs(a) * regs(b)
    case 3 => regs(a) * b

    case 4 => regs(a) & regs(b)
    case 5 => regs(a) & b

    case 6 => regs(a) | regs(b)
    case 7 => regs(a) | b

    case 8 => regs(a)
    case 9 => a

    case 10 => if (a > regs(b)) 1 else 0
    case 11 => if (regs(a) > b) 1 else 0
    case 12 => if (regs(a) > regs(b)) 1 else 0

    case 13 => if (a == regs(b)) 1 else 0
    case 14 => if (regs(a) == b) 1 else 0
    case 15 => if (regs(a) == regs(b)) 1 else 0
  })
}
val allOps = 0 to 15 map {MyOp(_)}

val (testcases, instructions) = {
  val input = scala.io.Source.fromFile("input").getLines.toVector
  val instructionPos = input.lastIndexWhere { x => TESTCASE_REGEX.findFirstMatchIn(x).isDefined } + 1
  val (unparsedTestCases, unparsedInstructions) = input.splitAt(instructionPos)

  val testcases =
    unparsedTestCases.sliding(3).collect {
      case Seq(TESTCASE_REGEX("Before", input),
               OPCODE_REGEX(v),
               TESTCASE_REGEX("After", output)) =>
        TestCase(inRegs = input.split(", ").map(_.toInt).toVector,
                 outRegs = output.split(", ").map(_.toInt).toVector,
                 opcode = v.split(" ").map(_.toInt).toVector)
    }.toVector
  val instructions = unparsedInstructions.collect {
    case OPCODE_REGEX(v) => v.split(" ").map(_.toInt).toVector
  }
  (testcases, instructions)
}

def matchingOps(tc: TestCase) = allOps.filter { op =>
    operation(tc.inRegs, op, tc.opcode) == tc.outRegs
  }

@scala.annotation.tailrec
def removeKnownSolutions[T](curr: Map[T, Set[MyOp]], last: Map[T, Set[MyOp]] = Map.empty[T, Nothing]): Map[T, Set[MyOp]] = {
  if (curr == last) curr
  else {
    val singles = curr.values.filter(_.size == 1).reduce((a, b) => a | b)
    removeKnownSolutions(curr.mapValues { v =>
      if (v.size == 1) v
      else v &~ singles
    }, curr)
  }
}

def determineOps(tcs: Seq[TestCase]) = {
  val byOpcode = tcs.groupBy(_.opcode(0))
  val opcodeMeanings = removeKnownSolutions(byOpcode.mapValues(_.foldLeft(allOps.toSet)((acc, tc) => acc & matchingOps(tc).toSet)))
  assert(opcodeMeanings.values.forall(_.size == 1))
  opcodeMeanings.mapValues(_.head)
}

def part1 = testcases.filter { t => matchingOps(t).size >= 3 }.size
def part2 = {
  val realOps = determineOps(testcases)
  val finalRegs = instructions.foldLeft(Vector(0, 0, 0, 0)) { (regs, op) => operation(regs, realOps(op(0)), op) }
  finalRegs(0)
}

println(part1)
println(part2)
