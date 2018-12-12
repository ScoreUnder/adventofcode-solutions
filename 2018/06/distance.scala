val coords = scala.io.Source.fromFile("input").getLines.map(_.split(",").map(_.trim)).collect {
  case Array(x, y) => (x.toInt, y.toInt)
}.toVector

class Array2D[T](val width: Int, val height: Int)(implicit ev: Manifest[T]) extends Iterable[T] {
  private[this] val back = new Array[T](width*height)

  def update(x: Int, y: Int, v: T) = back(y*width + x) = v
  def apply(x: Int, y: Int) = back(y*width + x)
  def iterator = back.iterator
}

def part1 = {
  val (width, height) = {
    val (xs, ys) = coords.unzip
    (xs.max + 2, ys.max + 2)
  }

  val FIELD_EMPTY = 0
  val FIELD_MULTIPLE = -1
  val field = new Array2D[Int](width, height)
  val claims = new Array2D[Boolean](width, height)

  def diamond(x: Int, y: Int, radius: Int): Seq[(Int, Int)] = {
    if (radius == 0) {
      List((x, y))
    } else {
      val half = (0 to radius).map { n => (radius - n, n) } ++ (1 until radius).map { n => (radius - n, -n) }
      (half ++ half.map { c => (-c._1, -c._2) }).map { c => (c._1 + x, c._2 + y) }
    }
  }

  def extendDistance(radius: Int): Boolean = {
    var changes = false
    for (((ox, oy), oid) <- coords.zipWithIndex;
         id = oid + 1;
         (x, y) <- diamond(ox + 1, oy + 1, radius)
         if x >= 0 && y >= 0 && x < width && y < height && !claims(x, y)) {

      changes = true
      field(x, y) match {
        case FIELD_EMPTY => field(x, y) = id
        case `id` =>
        case _ => field(x, y) = FIELD_MULTIPLE
      }
    }
    changes
  }

  def claimDistance(radius: Int) {
    for ((ox, oy) <- coords;
         (x, y) <- diamond(ox + 1, oy + 1, radius)
         if x >= 0 && y >= 0 && x < width && y < height) {
      claims(x, y) = true
    }
  }

  Stream.from(0) find { radius =>
    val changes = extendDistance(radius)
    claimDistance(radius)
    !changes
  }

  val disqualified = collection.mutable.HashSet[Int](FIELD_EMPTY, FIELD_MULTIPLE)

  for (x <- 0 until width) {
    disqualified += field(x, 0)
    disqualified += field(x, height - 1)
  }

  for (y <- 0 until height) {
    disqualified += field(0, y)
    disqualified += field(width - 1, y)
  }

  val largestArea = field.groupBy(identity).mapValues(_.size).filter(t => !disqualified.contains(t._1)).maxBy(_._2)
  largestArea._2
}

def part2 = {
  def totalDistance(x: Int, y: Int) = {
    coords.map { case (x2, y2) => (x - x2).abs + (y - y2).abs }.sum
  }

  val maxX = coords.map(_._1).max
  val maxY = coords.map(_._2).max

  (for (x <- 0 to maxX; y <- 0 to maxY)
    yield totalDistance(x, y)).count(_ < 10000)
}

println(part1)
println(part2)
