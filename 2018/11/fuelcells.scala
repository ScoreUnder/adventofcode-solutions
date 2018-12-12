val gridSerial = 7400

def powerByCoord(x: Int, y: Int) = {
  // Note: working with off-by-one coords due to problem spec
  val rackId = x + 10
  (rackId * y + gridSerial) * rackId / 100 % 10 - 5
}

val powerGrid =
  for (y <- 1 to 300)
    yield for (x <- 1 to 300)
      yield powerByCoord(x, y)

implicit class MyRichSeq(val me: Seq[Int]) {
  def zipSums(sumLen: Int) =
    me.sliding(sumLen, 1).map(_.sum).toVector
}

implicit class MyRichSeqSeq(val me: Seq[Seq[Int]]) {
  def clusterSums(size: Int) =
    me.map(_.zipSums(size)).transpose.map(_.zipSums(size)).transpose
}

def maxPower(clustered: Seq[Seq[Int]]) = {
  val ((power, col), row) = clustered.map(_.zipWithIndex.maxBy(_._1)).zipWithIndex.maxBy(_._1._1)
  (power, col, row)
}

def part1() {
  val (power, col, row) = maxPower(powerGrid clusterSums 3)
  println(s"Max power $power at (${col+1},${row+1})")
}

def part2() = {
  val maxPowers =
    for (size <- (1 to 300).par)
      yield (size, maxPower(powerGrid clusterSums size))
  val (size, (power, col, row)) = maxPowers.maxBy(_._2._1)
  println(s"Max power $power at (${col+1},${row+1},$size)")
}

part1()
part2()
