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

println(part1)
