import scala.annotation._

val recipes = collection.mutable.ArrayBuffer[Int](3, 7)
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

def nextTen(after: Int) = {
  while (recipes.size < after + 10) {
    step()
  }
  recipes drop after take 10
}

println(nextTen(702831).mkString)
