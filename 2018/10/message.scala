val LINE_REGEX = raw"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>".r

case class Particle(x: Int, y: Int, dx: Int, dy: Int)
case class Point(x: Int, y: Int)

val particles = scala.io.Source.fromFile("input").getLines.collect {
  case LINE_REGEX(x, y, dx, dy) => Particle(x.toInt, y.toInt, dx.toInt, dy.toInt)
}.toVector

def simulate(t: Int) =
  particles.map(p => Point(p.x + p.dx * t, p.y + p.dy * t))

def verticalLines(ps: Iterable[Point]) = {
  val threshold = 7
  val psSet = ps.toSet
  ps.count { p =>
    !psSet.contains(p.copy(y = p.y - 1)) &&
      (1 to threshold).forall { n => psSet.contains(p.copy(y = p.y + n)) }
  }
}

def seemsLegit(ps: Iterable[Point]) = {
  val threshold = 3
  verticalLines(ps) >= threshold
}

def drawPointCloud(ps: Seq[Point]) = {
  val sortedPoints = ps.distinct.sortWith { (a, b) => if (a.y == b.y) a.x < b.x else a.y < b.y }
  var baseCol = ps.map(_.x).min

  var row = sortedPoints(0).y
  var col = baseCol

  for (p <- sortedPoints) {
    if (row != p.y) {
      print("\n" * (p.y - row))
      row = p.y
      col = baseCol
    }
    if (col != p.x) {
      print(" " * (p.x - col))
      col = p.x
    }
    print("#")
    col += 1
  }
  println()
}

for (time <- (0 to 100000).par) {
  val snapshot = simulate(time)
  if (seemsLegit(snapshot)) {
    println(s"Found candidate at time $time")
    drawPointCloud(snapshot)
  }
}
