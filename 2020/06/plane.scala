#!/usr/bin/env scala
//!#

val questions = io.Source.fromFile("input").getLines()
  .foldLeft(List(List.empty[String])) { (acc, v) =>
    if (v.isEmpty) Nil :: acc
    else (v :: acc.head) :: acc.tail
  }

def part1 = questions.map(_.flatten.toSet.size).sum
def part2 = questions.map(_.map(_.toSet).reduce(_ intersect _).size).sum

println(part1)
println(part2)
