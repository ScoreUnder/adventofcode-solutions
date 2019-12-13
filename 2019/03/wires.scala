val directions = Map(
  "L" -> Vector(-1, 0),
  "R" -> Vector(1, 0),
  "U" -> Vector(0, -1),
  "D" -> Vector(0, 1),
)

implicit class VecMath[T : Numeric](val me: Seq[T]) {
  import Numeric.Implicits._
  def *%(scalar: T) = me.map(_ * scalar)
  def +%(other: Seq[T]) = (me zip other).map(t => t._1 + t._2)
  def manhattan = me.map(_.abs).sum
}

val wires =
  (for (line <- io.Source.fromFile("input").getLines)
    yield line.split(",").map(_.splitAt(1)).collect {
      case (dir, len) => (directions(dir), len.toInt)
    }).toSeq

val wiresExtended = wires.map { wire =>
  wire.foldLeft(Vector(Vector(0, 0)): Seq[Seq[Int]]) { (hist, turn) =>
    val pos = hist.last
    val (dir, len) = turn
    hist ++ ((1 to len).map { dist => pos +% (dir *% dist) })
  }
}

val wirePositions = wiresExtended.map { _.drop(1).toSet }

val closestCross = wirePositions.flatten.groupBy(identity).collect {
  case (pos, xs) if xs.length > 1 => pos
}.toVector.sortBy(_.manhattan).headOption

println(s"$closestCross ${closestCross.map(_.manhattan)}")
