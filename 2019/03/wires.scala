// Note: Vectors are (x, y, d) where (x,y) is the position on the grid of wires
// and (d) is the length along the current wire.
type WirePos = Seq[Int]
type Wire = Seq[WirePos]
val directions = Map(
  "L" -> Vector(-1, 0, 1),
  "R" -> Vector(1, 0, 1),
  "U" -> Vector(0, -1, 1),
  "D" -> Vector(0, 1, 1),
)

implicit class VecMath[T : Numeric](val me: Seq[T]) {
  import Numeric.Implicits._
  def *%(scalar: T) = me.map(_ * scalar)
  def +%(other: Seq[T]) = (me zip other).map(t => t._1 + t._2)
  def manhattan = me.map(_.abs).sum
}

val wiresExtended = {
  val wires =
    (for (line <- io.Source.fromFile("input").getLines)
      yield line.split(",").map(_.splitAt(1)).collect {
        case (dir, len) => (directions(dir), len.toInt)
      }).toSeq

  wires.map { wire =>
    wire.foldLeft(Vector(Vector(0, 0, 0)): Wire) { (hist, turn) =>
      val pos = hist.last
      val (dir, len) = turn
      hist ++ ((1 to len).map { dist => pos +% (dir *% dist) })
    }
  }
}

val closestCross = {
  val wirePositions = wiresExtended.map { _.drop(1).map(_ dropRight 1).toSet }
  wirePositions.flatten.groupBy(identity).collect {
    case (pos, xs) if xs.length > 1 => pos
  }.toVector.sortBy(_.manhattan).headOption
}

println(s"$closestCross ${closestCross.map(_.manhattan)}")

def excludeSelfCrossovers(positions: Seq[Wire]) =
  positions.map { wire =>
    wire.drop(1).foldLeft((List.empty[WirePos], Set.empty[WirePos])) { (acc, pos) =>
      val (hist, seen) = acc
      val rawPos = pos dropRight 1
      if (seen contains rawPos)
        (hist, seen)
      else
        (pos :: hist, seen + rawPos)
    }._1
  }

val closestCrossByWire =
  excludeSelfCrossovers(wiresExtended).flatten.groupBy(_ dropRight 1).collect {
    case (rawPos, positions) if positions.length > 1 =>
      positions.map(_.last)  // get wire distance
        .sorted.take(2).sum  // take sum of least 2
  }.minOption

println(closestCrossByWire)
