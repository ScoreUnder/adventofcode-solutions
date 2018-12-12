case class Rect(x: Int, y: Int, w: Int, h: Int, id: Int = 0) {
  def intersection(other: Rect): Rect = {
    val ix = x max other.x
    val iy = y max other.y
    val ix2 = x2 min other.x2
    val iy2 = y2 min other.y2
    Rect(ix, iy, ix2 - ix, iy2 - iy)
  }

  def x2 = x + w
  def y2 = y + h
  def valid = w > 0 && h > 0

  def splitall: Seq[(Int, Int)] =
    for (nx <- 0 until w;
         ny <- 0 until h)
       yield (x+nx, y+ny)
}

val LINE_FORMAT = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r

val rects = scala.io.Source.fromFile("input").getLines.collect {
  case LINE_FORMAT(id, x, y, w, h) => Rect(x.toInt, y.toInt, w.toInt, h.toInt, id.toInt)
}.toVector

//val rectByRight = collection.immutable.TreeMap(rects.view map { r => r.x + r.w -> r }: _*)
//val intersectingRects = rectByRight.values.view.flatMap({ r => rectByRight.from(r.x).values.view.filter(_ ne r).map(r.intersection(_).splitall).flatten }).toSet
//println(intersectingRects.size)

val claims = new collection.mutable.HashMap[(Int,Int),Int] withDefaultValue 0
for (r <- rects.view; sq <- r.splitall) {
  claims(sq) += 1
}
println(claims.values.count(_ > 1))
val nonOverlapped = rects.find(r => !rects.exists(s => (r ne s) && r.intersection(s).valid))
println(nonOverlapped)
