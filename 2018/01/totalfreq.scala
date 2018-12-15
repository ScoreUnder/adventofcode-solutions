val freqs = scala.io.Source.fromFile("freqs.txt").getLines.map(_.toLong).toVector
println(s"Total freq: ${freqs.sum}")
val seen = new scala.collection.mutable.HashSet[Long]
def infiniteFreqs = Iterator.continually(freqs).flatten
val firstRepeat = infiniteFreqs.scanLeft(0L)(_+_).find { x => !seen.add(x) }
println(s"First repeat element: $firstRepeat")
