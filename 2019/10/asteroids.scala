val asteroids = io.Source.fromFile("input").getLines.zipWithIndex.flatMap { case (line, lineInd) =>
  line.zipWithIndex.collect { case ('#', colInd) => (colInd, lineInd) }
}.toSeq

val maxDim = asteroids.map(_._1).max max asteroids.map(_._2).max

implicit class IntTupleOps(val me: (Int, Int)) {
  def +#(other: (Int, Int)) = (me._1 + other._1, me._2 + other._2)
  def -#(other: (Int, Int)) = (me._1 - other._1, me._2 - other._2)
  def /#(other: Int) = (me._1 / other, me._2 / other)
}

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
def normaliseDelta(delta: (Int, Int)) = delta /# gcd(delta._1, delta._2).abs

def repeatedAdd(base: (Int, Int), delta: (Int, Int)) =
  Iterator.iterate(base) { _ +# delta }

def withinField(pos: (Int, Int)) =
  (pos._1 min pos._2) >= 0 && (pos._1 max pos._2) <= maxDim

def part1 = {
  (for (chosenAsteroid <- asteroids) yield {
    val blocked = (for (otherAsteroid <- asteroids.view if otherAsteroid != chosenAsteroid) yield {
      val delta = normaliseDelta(otherAsteroid -# chosenAsteroid)
      repeatedAdd(otherAsteroid, delta) drop 1 takeWhile withinField
    }).flatten.toSet
    chosenAsteroid -> (asteroids.filter(!blocked.contains(_)).size - 1)
  }).maxBy(_._2)
}

println(part1)
