import scala.collection.mutable.{ArrayBuffer, Queue}

val X_YRANGE_REGEX = """x=(\d+), y=(\d+)\.\.(\d+)""".r
val Y_XRANGE_REGEX = """y=(\d+), x=(\d+)\.\.(\d+)""".r

object IntStr {
  def unapply(x: String): Option[Int] = try Some(x.toInt) catch { case _ : NumberFormatException => None }
}

val (minX, minY, maxX, maxY, field) = {
  val coords = scala.io.Source.fromFile("input").getLines.collect {
    case X_YRANGE_REGEX(IntStr(x), IntStr(y1), IntStr(y2)) => (y1 to y2).map(y => (x, y))
    case Y_XRANGE_REGEX(IntStr(y), IntStr(x1), IntStr(x2)) => (x1 to x2) map(x => (x, y))
  }.flatten.toVector.view

  val minX = coords.map(_._1).min - 1  // Pad x-coords by 1 to allow runoff on the edges
  val maxX = coords.map(_._1).max + 1
  val minY = coords.map(_._2).min
  val maxY = coords.map(_._2).max

  val field = ArrayBuffer.fill(maxY-minY + 1)(ArrayBuffer.fill(maxX-minX + 1)('.'))

  for ((x, y) <- coords) {
    field(y - minY)(x - minX) = '#'
  }

  (minX, minY, maxX, maxY, field)
}

def inBounds(x: Int, y: Int) = x >= minX && x <= maxX && y >= minY && y <= maxY

def chrIsWater(c: Char) = c match {
  case '~' | '|' => true
  case _ => false
}

def passable(x: Int, y: Int) = {
  if (inBounds(x, y)) {
    val c = field(y - minY)(x - minX)
    c match {
      case '|' | '.' => true
      case _ => false
    }
  } else true
}

def canFill(x: Int, y: Int): Boolean = {
  assert(passable(x, y) && inBounds(x, y))

  var x2 = x

  // Ensure a floor exists up to the right wall
  while (passable(x2, y)) {
    if (passable(x2, y + 1) && inBounds(x2, y + 1))
      return false
    x2 += 1
  }

  x2 = x
  // Ensure a floor exists up to the left wall
  while (passable(x2, y)) {
    if (passable(x2, y + 1) && inBounds(x2, y + 1))
      return false
    x2 -= 1
  }

  return true
}

def fill(x: Int, y: Int) {
  val Some(xleft) = Iterator.from(x, -1) find { x2 => !passable(x2, y) }
  val Some(xright) = Iterator.from(x) find { x2 => !passable(x2, y) }
  val row = field(y - minY)
  ((xleft + 1) to (xright - 1)) foreach { x2 => row(x2 - minX) = '~' }
}

def tick(coord: (Int, Int, Int)) = {
  val (x, y, dir) = coord
  println(s"tick $coord")
  if (inBounds(x, y) && passable(x, y)) {
    if (passable(x, y + 1)) {
      field(y - minY)(x - minX) = '|'
      List((x, y + 1, 0))
    } else {
      if (canFill(x, y)) {
        fill(x, y)
        List((x, y - 1, 0))
      } else {
        field(y - minY)(x - minX) = '|'
        (dir match {
          case 0 => List((x - 1, y, 1), (x + 1, y, 2))
          case 1 => List((x - 1, y, 1))
          case 2 => List((x + 1, y, 2))
        }).filter(t => passable(t._1, t._2))
      }
    }
  } else Nil
}

def allTicks() {
  val remaining = Queue[(Int, Int, Int)]((500, minY, 0))
  while (remaining.nonEmpty) {
    remaining ++= tick(remaining.dequeue())
  }
}

allTicks()
println(field.map(_.mkString).mkString("\n"))

def part1 = field.view.flatten.count(chrIsWater)
def part2 = field.view.flatten.count(_ == '~')

println(part1)
println(part2)
