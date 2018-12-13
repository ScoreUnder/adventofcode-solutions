val INITIAL_STATE_REGEX = "initial state: ([.#]+)".r
val RULE_REGEX = "([.#]{5}) => ([.#])".r

val (initialState, rules) = {
  val instructions = scala.io.Source.fromFile("input").getLines.toVector

  (instructions.collect { case INITIAL_STATE_REGEX(state) => state.toVector }.ensuring(_.size == 1).head,
   instructions.collect { case RULE_REGEX(left, right) => left.toVector -> right.ensuring(_.size == 1).head }.toMap[Seq[Char],Char] withDefaultValue '.')
}

implicit class RichVector[T](val me: Vector[T]) {
  def dropRightWhile(f: T => Boolean): Vector[T] = me take (me.lastIndexWhere(!f(_)) + 1)
}

val PADDING = Vector.fill(4)('.')  // 2 to allow growth up to 2 tiles left/right, plus 2 to pad out the sliding window
case class SimState(offset: Int, plants: Vector[Char]) {
  def padded = copy(offset = offset + PADDING.size, plants = PADDING ++ plants ++ PADDING)
  def trimmed = copy(offset = offset - plants.indexOf('#'), plants = plants.dropWhile(_ == '.').dropRightWhile(_ == '.'))
}

val simStream = Stream.iterate(SimState(offset = 0, plants = initialState).trimmed) { state =>
  val newState = state.padded
  val newPlants = newState.plants.sliding(5).map(rules(_)).toVector
  newState.copy(offset = newState.offset - 2, plants = newPlants).trimmed
}

def printState(state: SimState) = {
  print(" " * (3 - state.offset))
  println(state.plants.mkString)
}

def sumPotIndices(state: SimState) = {
  state.plants.view.zipWithIndex.collect {
    case ('#', ind) => ind - state.offset
  }.sum
}

def part1 = {
  simStream take 21 foreach printState
  sumPotIndices(simStream(20))
}

def part2 = {
  // Scan for equilibrium (plants all the same, just "moving" along)
  val eqPoint = simStream.drop(1).zip(simStream).zipWithIndex.collect {
    case ((a, b), ind) if a.plants == b.plants => ind
  }.head

  // Fast forward to the right point in time
  val target = BigInt("50000000000")
  val offsetDiff = simStream(eqPoint + 1).offset - simStream(eqPoint).offset
  val timeDiff = target - eqPoint
  sumPotIndices(simStream(eqPoint)) - timeDiff * offsetDiff * simStream(eqPoint).plants.count(_ == '#')
}

println(part1)
println(part2)
