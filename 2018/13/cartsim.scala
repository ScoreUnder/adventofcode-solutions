case class Vec2(x: Int, y: Int) {
  def +(other: Vec2) = Vec2(x + other.x, y + other.y)
}

class Field(field: IndexedSeq[IndexedSeq[Char]]) {
  def apply(pos: Vec2) = field(pos.y)(pos.x)
}

case class Cart(position: Vec2, direction: Vec2, turnStatus: Int = 0) extends Ordered[Cart] {
  def tick(field: Field) = {
    val nextPosition = position + direction
    val nextTrack = field(nextPosition) ensuring (_ != ' ')
    val nextDirection = nextTrack match {
      case '\\' => Vec2(direction.y, direction.x)
      case '/' => Vec2(-direction.y, -direction.x)
      case '+' => turnStatus match {
        case 0 => Vec2(direction.y, -direction.x)
        case 1 => direction
        case 2 => Vec2(-direction.y, direction.x)
      }
      case _ => direction
    }
    val nextTurnStatus =
      if (nextTrack == '+') (turnStatus + 1) % 3
      else turnStatus

    Cart(nextPosition, nextDirection, nextTurnStatus)
  }

  override def compare(other: Cart) =
    if (position.y == other.position.y) position.x - other.position.x
    else position.y - other.position.y
}

val (field, initialCarts) = {
  val chrToDir = Map(
    '<' -> ('-', Vec2(-1, 0)),
    '>' -> ('-', Vec2(1, 0)),
    '^' -> ('|', Vec2(0, -1)),
    'v' -> ('|', Vec2(0, 1))
  )
  val (field, carts) = scala.io.Source.fromFile("input").getLines.zipWithIndex.map { case (row, y) =>
    row.zipWithIndex.map { case (chr, x) =>
      chrToDir.get(chr) match {
        case Some((track, dir)) => (track, Some(Cart(Vec2(x, y), dir)))
        case None => (chr, None)
      }
    }.unzip
  }.toVector.unzip

  (new Field(field), carts.flatten.flatten)
}


def findCollisions(positions: Seq[Vec2]) =
  positions.diff(positions.distinct)

def cartSim(removeCollisions: Boolean) = Stream.iterate(Vector(initialCarts)) { carts =>
  var cartsByTurn = carts.last.sorted
  val subticks = new scala.collection.mutable.ArrayBuffer[Vector[Cart]]

  var i = 0
  while (i < cartsByTurn.size) {
    cartsByTurn = cartsByTurn.updated(i, cartsByTurn(i).tick(field))
    if (removeCollisions) {
      val collisions = findCollisions(cartsByTurn.map(_.position))
      i -= cartsByTurn.zipWithIndex.collect { case (c, ind) if collisions.contains(c.position) => ind }.count(_ <= i)
      cartsByTurn = cartsByTurn.filter { c => !collisions.contains(c.position) }
    }
    subticks += cartsByTurn
    i += 1
  }

  subticks.toVector
}.flatten

def part1 = cartSim(removeCollisions = false).map(c => findCollisions(c.map(_.position))).filter(_.nonEmpty)(0)(0)
def part2 = cartSim(removeCollisions = true).filter { _.size == 1 }(1)(0).position

println(part1)
println(part2)
