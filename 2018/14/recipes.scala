import scala.annotation._

val recipes = collection.mutable.ArrayBuffer[Int](3, 7)
recipes.sizeHint(30000000)
var elves = Vector(0, 1)

@tailrec
def numToDigits(n: Int, acc: List[Int] = Nil): List[Int] =
  n match {
    case 0 => if (acc == Nil) List(0) else acc
    case _ => numToDigits(n / 10, n % 10 :: acc)
  }

def step() = {
  val recipeScore = elves.map(recipes(_)).sum
  recipes ++= numToDigits(recipeScore)
  elves = elves.map(e => (e + recipes(e) + 1) % recipes.size)
  recipeScore
}

def infiniteRecipes = new Iterator[Int] {
  var i = 0
  override def next = {
    while (i >= recipes.size) {
      step()
    }
    i += 1
    recipes(i - 1)
  }
  override def hasNext = true
}

def nextTen(after: Int) = infiniteRecipes drop after take 10

def recipesIndex(compare: Seq[Int]) =
  infiniteRecipes.sliding(compare.size).zipWithIndex.collect {
    case (lst, ind) if lst == compare => ind
  }.next

val input = 702831
def part1 = nextTen(input).mkString
def part2 = recipesIndex(numToDigits(input))

println(part1)
println(part2)
