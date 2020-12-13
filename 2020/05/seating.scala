#!/usr/bin/env scala
//!#

def decodePos(str: String, rowStart: Int = 0, rowLen: Int = 127, colStart: Int = 0, colLen: Int = 7): (Int, Int) =
  if (colLen == 0 && rowLen == 0) (colStart, rowStart)
  else str.head match {
    case 'F' => decodePos(str.tail, rowStart, rowLen / 2, colStart, colLen)
    case 'B' => decodePos(str.tail, rowStart + rowLen - rowLen / 2, rowLen / 2, colStart, colLen)
    case 'R' => decodePos(str.tail, rowStart, rowLen, colStart + colLen - colLen / 2, colLen / 2)
    case 'L' => decodePos(str.tail, rowStart, rowLen, colStart, colLen / 2)
    case _ => decodePos(str.tail, rowStart, rowLen, colStart, colLen)
  }

def toSeatId(pos: (Int, Int)) = pos._2 * 8 + pos._1

val seats = io.Source.fromFile("input").getLines().map(l => toSeatId(decodePos(l))).toVector

def part1 = seats.max
def part2 = {
  val ss = seats.sorted
  ss.zip(ss.drop(1)).find(s => s._1 + 1 != s._2)
}

println(part1)
println(part2)
