val masses = scala.io.Source.fromFile("input").getLines.map(_.toInt).toSeq

def massToFuel(mass: Int) = 0 max (mass/3 - 2)
println(masses.map(massToFuel).sum)

def cascadeFuel(initial: Int) = {
  @annotation.tailrec
  def inner(mass: Int, acc: Int): Int = {
    massToFuel(mass) match {
      case 0 => acc
      case fuel => inner(fuel, fuel + acc)
    }
  }
  inner(initial, 0)
}
println(masses.map(cascadeFuel).sum)
