val ids = scala.io.Source.fromFile("input").getLines.toVector

def bool2int(x: Boolean) = if (x) 1 else 0
val (twos, threes) = ids.map { id =>
  val counts = id.groupBy(identity).values.view.map(_.size).toSet
  (bool2int(counts contains 2), bool2int(counts contains 3))
}.fold((0, 0)) { (a, b) => (a._1+b._1, a._2+b._2) }
println(s"Checksum: ${twos * threes}")

def equal(t: (Any, Any)) = t._1 == t._2
def differences(x: Seq[_], y: Seq[_]) = x zip y map equal count(!_)
val closeIds = ids.view.map { x =>
  ids.find { y => differences(x, y) == 1 }.map((x, _))
}.filter(_.isDefined).headOption.flatten
val commonLetters = closeIds.map {
  case (a, b) => (a zip b map equal) zip a collect { case (true, x) => x } mkString ""
}
println(s"Letters in common between closest IDs: $commonLetters")
