val freqs = scala.io.Source.fromFile("freqs.txt").getLines.map(_.toLong).toVector
println(s"Total freq: ${freqs.sum}")
val seen = new scala.collection.mutable.HashSet[Long]
val infiniteFreqs: Stream[Long] = freqs.toStream #::: infiniteFreqs
val firstRepeat = infiniteFreqs.scanLeft(0L)(_+_).find { x =>
  val stop = seen contains x
  seen += x
  stop
}
println(s"First repeat element: $firstRepeat")
