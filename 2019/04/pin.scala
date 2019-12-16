// Question input:
val (minPin, maxPin) = (234208, 765869)

implicit class RichNum(val me: Int) {
  def digits = {
    require(me > 0)
    @annotation.tailrec
    def digits0(num: Int, acc: List[Int]): List[Int] =
      if (num == 0) acc
      else digits0(num / 10, (num % 10) :: acc)
    digits0(me, Nil)
  }
}

def ascendingLists(min: Int, max: Int, len: Int): Seq[List[Int]] =
  if (len == 1) (min to max).map(List(_))
  else (min to max).flatMap { digit =>
    ascendingLists(digit, max, len - 1).map { lst => digit :: lst }
  }

def seqWithDuplicates[T](input: Seq[T]) =
  for (n <- input.indices)
    yield input.take(n+1) ++ input.drop(n)

def digitsToInt(digits: Seq[Int]) =
  digits.foldLeft(0) { (acc, n) => 10*acc + n }

val maxStartDigit = {
  val headDig :: tailDig = maxPin.digits
  if (tailDig.exists(_ < headDig)) headDig - 1
  else headDig
}
val minStartDigit = minPin.digits.head
val pinLen = maxPin.digits.length

val candidates =
  ascendingLists(minStartDigit, 9, pinLen - 1)
    .view
    .filter(_.head <= maxStartDigit)
    .flatMap(seqWithDuplicates)
    .map(digitsToInt)
    .filter(n => n >= minPin && n <= maxPin)
    .toSet

println(candidates.size)
