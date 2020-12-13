#!/usr/bin/env scala
val nums = io.Source.fromFile("input").getLines().flatMap(_.trim.toIntOption).toVector

def part1() = {
  for (n <- nums;
       m <- nums) {
    if (n + m == 2020) {
      println(s"$n + $m = 2020; product = ${n * m}")
    }
  }
}

def part2() = {
  for (n <- nums;
       m <- nums;
       o <- nums) {
    if (n + m + o == 2020) {
      println(s"$n + $m + $o = 2020; product = ${n * m * o}")
    }
  }
}

part1()
part2()