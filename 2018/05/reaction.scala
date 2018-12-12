import scala.annotation.tailrec
val full = scala.io.Source.fromFile("input").filter(_ != '\n').toList

@tailrec def reactReverse(poly: List[Char], acc: List[Char] = Nil): List[Char] = {
  (poly, acc) match {
    case (p1 :: polyt, a1 :: acct) if (p1 ^ 0x20) == a1 => reactReverse(polyt, acct)
    case (p1 :: polyt, acc) => reactReverse(polyt, p1 :: acc)
    case (Nil, acc) => acc
  }
}

println(reactReverse(full).size)

println((('a' to 'z').view map { c => reactReverse(full.filter(c2 => (c2 | 0x20) != c)).size }).min)
