val width = 25
val height = 6
val image = io.Source.fromFile("input").getLines.mkString.grouped(width).grouped(height).toSeq

// image → layers → rows → pixels
assert(image.last.last.length == image.head.last.length)
assert(image.last.length == image.head.length)

def part1 = {
  val leastZeros = image.map(_.flatten).minBy { _.count(_ == '0') }
  leastZeros.count(_ == '1') * leastZeros.count(_ == '2')
}

def part2 = {
  // layers-of-rows-of-pixels
  // →transpose→ rows-of-layers-of-pixels
  // →map(transpose)→ rows-of-pixels-of-layers
  // with layers accessible per-pixel, transparency is pretty easy:
  // just find the first non-transparent pixel-layer.
  val layersMerged = image.transpose.map(_.transpose.map(_.find(_ != '2').getOrElse('0')))
  layersMerged.map(_.map {
    case '1' => '#'
    case '0' => ' '
  }.mkString).mkString("\n")
}

println(part1)
println(part2)
