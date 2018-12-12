import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class CycleList(val value: Long, var prev: CycleList, var next: CycleList) {
  @tailrec
  final def travel(n: Long): CycleList =
    if (n == 0) this
    else if (n < 0) prev.travel(n + 1)
    else next.travel(n - 1)

  def remove(): CycleList = {
    next.prev = prev
    prev.next = next
    next
  }

  def insert(value: Long): CycleList = {
    prev = new CycleList(value, prev, this)
    prev.prev.next = prev
    prev
  }
}

def zeroCycleList = {
  val list = new CycleList(0, null, null)
  list.prev = list
  list.next = list
  list
}

class MarbleGame(val players: Int) {
  private var marbles = zeroCycleList
  private val _scores = ArrayBuffer.fill(players)(0L)
  private var turn = 1

  def scores = _scores.toVector

  def takeTurn() {
    if (turn % 23 == 0) {
      marbles = marbles travel -9
      _scores(turn % players) += marbles.value + turn
      marbles = marbles.remove()
    } else {
      marbles = marbles.insert(turn)
    }

    turn += 1
    marbles = marbles travel 2
  }
}

def play(turns: Int) = {
  val game = new MarbleGame(419)
  for (_ <- 0 until turns)
    game.takeTurn()
  game.scores.max
}

def part1 = play(72164)
def part2 = play(7216400)

println(part1)
println(part2)
