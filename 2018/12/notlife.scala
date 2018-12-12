val INITIAL_STATE_REGEX = "initial state: ([.#]+)".r
val RULE_REGEX = "([.#]{5}) => ([.#])".r

val (initialState, rules) = {
  val instructions = scala.io.Source.fromFile("input").getLines.collect {
    case INITIAL_STATE_REGEX(state) => Left(state.toVector)
    case RULE_REGEX(left, right) => Right(left.toVector -> right.ensuring(_.size == 1).head)
  }.toVector

  (instructions.collect { case Left(x) => x }.ensuring(_.size == 1).head,
   instructions.collect { case Right(x) => x }.toMap[Seq[Char],Char] withDefaultValue '.')
}

val PADDING = Vector.fill(4)('.')  // 2 to allow growth up to 2 tiles left/right, plus 2 to pad out the sliding window
case class SimState(offset: Int, plants: Seq[Char]) {
  def padded =
    (plants take PADDING.size, plants takeRight PADDING.size) match {
      case (PADDING, PADDING) => this
      case (_, PADDING) => copy(offset = offset + PADDING.size, plants = PADDING ++ plants)
      case (PADDING, _) => copy(plants = plants ++ PADDING)
      case (_, _) => copy(offset = offset + PADDING.size, plants = PADDING ++ plants ++ PADDING)
    }
}

implicit class RichVector[T](val me: Vector[T]) {
  def dropRightWhile(f: T => Boolean): Vector[T] = me take (me.lastIndexWhere(!f(_)) + 1)
}

val simStream = Stream.iterate(SimState(offset = 0, plants = initialState)) { state =>
  val newState = state.padded
  val newPlants = newState.plants.sliding(5).map(rules(_)).toVector.dropRightWhile(_ == '.')
  val trimmedPlants = newPlants.dropWhile(_ == '.')
  newState.copy(offset = newState.offset - 2 - (newPlants.size - trimmedPlants.size), plants = trimmedPlants)
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
