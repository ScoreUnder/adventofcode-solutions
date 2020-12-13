#!/usr/bin/env scala
//!#

class Field(field: IndexedSeq[String]) {
  def occupiedAt(x: Int, y: Int) = {
    val row = field(y)
    row(x % row.length) == '#'
  }

  def rows = field.size
}

val field = new Field(io.Source.fromFile("input").getLines().toVector)

def countTreesForSlope(slope: (Int, Int)) = Iterator
  .iterate((0, 0)) { p => (p._1 + slope._1, p._2 + slope._2) }
  .takeWhile(p => p._2 < field.rows)
  .count((field.occupiedAt _).tupled)

def part1() = println(countTreesForSlope((3, 1)))
def part2() = {
  val slopes = List(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2),
  )
  println(slopes.map(countTreesForSlope).product)
}

part1()
part2()
