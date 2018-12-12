import scala.annotation.tailrec
import scala.collection.{immutable, mutable, Set => AnySet}

val STEP_REGEX = raw"Step (\w+) must be finished before step (\w+) can begin\.".r

val (reverseDeps, allSymbols) = {
  val reverseDepTuples = scala.io.Source.fromFile("input").getLines.collect {
    case STEP_REGEX(depTarget, depSource) => (Symbol(depTarget), Symbol(depSource))
  }.toVector.groupBy(_._2) withDefaultValue Vector.empty

  implicit val symbolOrder = Ordering.by[Symbol, String](_.name)

  (reverseDepTuples.mapValues(_.map(_._1)) withDefaultValue Vector.empty,
   reverseDepTuples.valuesIterator.flatten.map(d => List(d._1, d._2)).flatten.to[immutable.SortedSet])
}

def satisfied(completeDeps: AnySet[Symbol])(sym: Symbol) =
  reverseDeps(sym) forall { completeDeps.contains(_) }

def nextUnsatisfied(completeDeps: AnySet[Symbol], ignoreDeps: AnySet[Symbol] = Set.empty) =
  allSymbols.filter(t => !completeDeps.contains(t) && !ignoreDeps.contains(t)).find(satisfied(completeDeps))

def part1 = {
  val completeDeps = new mutable.LinkedHashSet[Symbol]

  @tailrec def populate() {
    nextUnsatisfied(completeDeps) match {
      case Some(sym) =>
        completeDeps += sym
        populate()
      case None =>
    }
  }
  populate()

  (completeDeps map { _.name }).mkString
}

implicit class MinByOption[T](val me: Iterable[T]) {
  def minByOption[U : Ordering](f: (T) => U): Option[T] =
    if (me.isEmpty) None
    else Some(me.minBy(f))
}

def part2 = {
  val completeDeps = new mutable.LinkedHashSet[Symbol]
  val depsInProgress = new mutable.ArrayBuffer[(Symbol, Int)]
  val workers = 5

  def workersAvailable = depsInProgress.size < workers
  def timeForTask(task: Symbol) = task.name(0) - 'A' + 61

  @tailrec def populate(tick: Int): Int = {
    (if (workersAvailable) nextUnsatisfied(completeDeps, depsInProgress.map(_._1).toSet) else None) match {
      case Some(sym) => // Workers and task available ==> assign one
        depsInProgress += sym -> (timeForTask(sym) + tick)
        populate(tick)
      case None => // No workers available, or no satisfiable deps ==> advance time instead
        depsInProgress.zipWithIndex.minByOption(_._1._2) match {
          case Some(((dep, tick2), index)) =>
            depsInProgress.remove(index)
            completeDeps += dep
            populate(tick2)
          case None =>
            tick
        }
    }
  }

  populate(0)
}

println(part1)
println(part2)
