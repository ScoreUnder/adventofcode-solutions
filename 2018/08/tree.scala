val rawData = scala.io.Source.fromFile("input").mkString.split(" ").map(_.trim.toInt).toVector // Note: Array.iterator has a buggy implementation of take

case class Node(children: Seq[Node], meta: Seq[Int]) extends Iterable[Node] {
  def iterator = (children.toStream.map(_.iterator).flatten :+ this).iterator
}

def parseTree(iter: Iterator[Int]): Node = {
  val childCount = iter.next()
  val metaCount = iter.next()
  val children = (0 until childCount).map { _ => parseTree(iter) }.toVector
  val meta = iter.take(metaCount).toVector
  Node(children, meta)
}

val tree = parseTree(rawData.iterator)

println(tree.map(_.meta.sum).sum)

val data = new collection.mutable.HashMap[Node, Int]
for (node <- tree) {
  data += node -> (
    if (node.children.isEmpty) {
      node.meta.sum
    } else {
      (for (meta <- node.meta if meta > 0 && meta <= node.children.size)
        yield data(node.children(meta - 1))
      ).sum
    }
  )
}
println(data(tree))
