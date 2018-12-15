import scala.collection.mutable.{HashMap, ArrayBuffer, Queue}

object Team extends Enumeration {
  type Team = Value
  val ELF, GOBLIN = Value
}
import Team._

case class Vec2(x: Int, y: Int) extends Ordered[Vec2] {
  def compare(other: Vec2) =
     if (y == other.y) x - other.x
     else y - other.y

  def +(other: Vec2) = Vec2(x + other.x, y + other.y)
  def -(other: Vec2) = Vec2(x - other.x, y - other.y)
  def distance(other: Vec2) = (x - other.x).abs + (y - other.y).abs
}

val directions = Vector(Vec2(-1, 0), Vec2(1, 0), Vec2(0, -1), Vec2(0, 1)).sorted
val reverseDirs = directions.reverse

case class Troop(pos: Vec2, team: Team, hp: Int = 200, atk: Int = 3)

trait Tile {
  def passable: Boolean
}

case object Floor extends Tile {
  override def passable = true
}

case object Wall extends Tile {
  override def passable = false
}

class Field(field: IndexedSeq[IndexedSeq[Char]]) extends Iterable[IndexedSeq[Char]] {
  def apply(pos: Vec2) =
    field(pos.y)(pos.x) match {
      case '.' => Floor
      case '#' => Wall
    }

  def iterator = field.iterator

  def width = field(0).size
  def height = field.size
}

def doAStar(pos: Vec2, accessible: (Vec2) => Boolean) = {
  val astarField = new HashMap[Vec2, Int] withDefaultValue Int.MaxValue
  val toCheck = Queue((pos, 0))

  while (toCheck.nonEmpty) {
    val (currPos, distance) = toCheck.dequeue()
    if (astarField(currPos) > distance) {
      astarField(currPos) = distance
      toCheck.enqueue(directions.map(d => (d + currPos, distance + 1)).filter(d => accessible(d._1)): _*)
    }
  }

  astarField.toMap withDefaultValue Int.MaxValue
}

def aStarPathTo(aStarDistances: Map[Vec2, Int], pos: Vec2, acc: List[Vec2] = Nil): List[Vec2] = {
  aStarDistances(pos) match {
    case Int.MaxValue => ???
    case 0 => acc
    case dist =>
      val nextMove = reverseDirs.filter(d => dist - aStarDistances(pos - d) == 1).head
      aStarPathTo(aStarDistances, pos - nextMove, nextMove :: acc)
  }
}

val (field, initialEntities) = {
  val (field, entitiesMaybe) = scala.io.Source.fromFile("input").getLines.zipWithIndex.map { case (row, y) =>
    row.zipWithIndex.map {
      case ('E', x) => ('.', Some(Troop(Vec2(x, y), ELF)))
      case ('G', x) => ('.', Some(Troop(Vec2(x, y), GOBLIN)))
      case (e, _) => (e, None)
    }.unzip
  }.toVector.unzip

  (new Field(field), entitiesMaybe.flatten.flatten)
}

def entitiesSimulation(initialEntities: Seq[Troop]) = Stream.iterate(initialEntities) { entities =>
  val newEntities = entities.sortBy { e => e.pos }.toBuffer
  // This is all imperative, oh well
  var i = 0
  while (i < newEntities.size) {
    var me = newEntities(i)

    val unitPositions = newEntities.map(_.pos).toSet
    val targets = newEntities.filter(_.team != me.team)
    def passable(p: Vec2) = field(p).passable && !unitPositions.contains(p)

    def tryAttack(): Boolean =
      targets.filter(t => (t.pos distance me.pos) == 1) match {
        case Seq() => false
        case Seq(targets @ _*) => // attack
          val myTarget = targets.minBy(_.hp)
          val ti = newEntities.indexOf(myTarget)
          val updatedTarget = myTarget.copy(hp = myTarget.hp - me.atk)
          if (updatedTarget.hp <= 0) {
            newEntities.remove(ti)
            if (ti <= i) i -= 1
          } else {
            newEntities(ti) = updatedTarget
          }
          true
      }

    if (!tryAttack()) {
      val targetAdj = targets.flatMap(t => directions.map(_ + t.pos)).filter(passable).sorted
      if (targetAdj.nonEmpty) {
        val aStarDistances = doAStar(me.pos, passable)
        val minDist = targetAdj.map(aStarDistances(_)).min
        targetAdj.filter(tp => minDist != Int.MaxValue && aStarDistances(tp) == minDist).headOption match {
          case Some(targetPos) => // Move in for attack
            val dir = aStarPathTo(aStarDistances, targetPos).head
            me = me.copy(pos = me.pos + dir)
            newEntities(i) = me
            tryAttack()
          case None => // Nothing to do
        }
      }
    }

    i += 1
  }

  newEntities.toVector
}

def printSim(field: Field, entities: Seq[Troop]) = {
  val entitiesByPos = entities.groupBy(_.pos).mapValues(_.head)
  for ((row, y) <- field.zipWithIndex) {
    val line = for ((chr, x) <- row.zipWithIndex) yield {
      entitiesByPos.get(Vec2(x, y)) match {
        case Some(Troop(_, ELF, _, _)) => 'E'
        case Some(Troop(_, GOBLIN, _, _)) => 'G'
        case None => chr
      }
    }
    println(line.mkString)
  }
}

def score(entities: Seq[Seq[Troop]], turn: Int) =
  entities(turn).map(_.hp).sum * (turn - 1)

def part1 = {
  val sim = entitiesSimulation(initialEntities)
  val finalTurn = sim.indexWhere(en => en.map(_.team).toSet.size != 2)
  score(sim, finalTurn)
}

def part2 = {
  val initialElves = initialEntities.count(_.team == ELF)
  (for (atk <- Iterator from 4) yield {
    val sim = entitiesSimulation(initialEntities.map {
        case e @ Troop(_, ELF, _, _) => e.copy(atk = atk)
        case g => g
      })

    val finalTurn = sim.indexWhere(en => en.map(_.team).toSet.size != 2 || en.count(_.team == ELF) != initialElves)
    if (sim(finalTurn).count(_.team == ELF) == initialElves) {
      Some(score(sim, finalTurn))
    }
    else None
  }).flatten.next
}

println(part1)
println(part2)
