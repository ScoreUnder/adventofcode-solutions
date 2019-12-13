val masses = scala.io.Source.fromFile("input").getLines.map(_.toInt).toSeq

println(masses.map(_/3 - 2).sum)
