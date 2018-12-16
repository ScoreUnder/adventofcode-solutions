val TESTCASE_REGEX = """^(\w+):\s+\[([^]]*)\]$""".r
val OPCODE_REGEX = """^(\d+(?:\s+\d+){3})$""".r

type Regs = Vector[Int]

case class MyOp(code: Int)
case class TestCase(inRegs: Regs, outRegs: Regs, opcode: Vector[Int])

def operation(regs: Regs, op: MyOp, a: Int, b: Int, c: Int): Regs =
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
    operation(tc.inRegs, op, tc.opcode(1), tc.opcode(2), tc.opcode(3)) == tc.outRegs
  }

def part1 = testcases.filter { t => matchingOps(t).size >= 3 }.size

println(part1)
