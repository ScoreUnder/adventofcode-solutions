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
