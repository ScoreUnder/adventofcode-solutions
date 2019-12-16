import collection.mutable
val orbits = io.Source.fromFile("input").getLines.map(_.split(')')).map { case Array(parent, child) => child -> parent }.toMap

def memoized[K, V](f: K => V): K => V = {
  val cache = mutable.Map.empty[K, V]
  k => cache.getOrElse(k, {
    val result = f(k)
    cache(k) = result
    result
  })
}

val getOrbitCount: String => Int = memoized { planet: String =>
  orbits.get(planet) match {
    case Some(orbit) => getOrbitCount(orbit) + 1
    case None => 0
  }
}

// Part 1
println(orbits.keys.view.map(getOrbitCount).sum)

def getTransferNum(from: String, to: String) = {
  @annotation.tailrec
  def gatherTransfers(from: String, to: String, acc: Int): Int =
    if (from == to) acc
    else if (getOrbitCount(from) > getOrbitCount(to)) gatherTransfers(orbits(from), to, acc + 1)
    else gatherTransfers(from, orbits(to), acc + 1)
  gatherTransfers(orbits(from), orbits(to), 0)
}

// Part 2
println(getTransferNum("YOU", "SAN"))
