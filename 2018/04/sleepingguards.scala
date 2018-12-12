import scala.collection.mutable

val TIME_REGEX = raw"\[[^]]*:(\d+)\] "  // Apparently only the minute matters
val GUARD_REGEX = (TIME_REGEX + raw"Guard #(\d+) begins shift").r
val SLEEP_REGEX = (TIME_REGEX + raw"falls asleep").r
val WAKE_REGEX = (TIME_REGEX + raw"wakes up").r

case class GuardChange(id: Int)
case class Sleep(time: Int)
case class Wake(time: Int)

class SleepPeriod(var total: Int = 0, val byMinute: Array[Int] = new Array[Int](60))

val events = scala.io.Source.fromFile("input").getLines.toVector.sorted.collect {
  case GUARD_REGEX(time, guard) => GuardChange(guard.toInt)
  case SLEEP_REGEX(time) => Sleep(time.toInt)
  case WAKE_REGEX(time) => Wake(time.toInt)
}

var guard = 0
var lastTime: Option[Int] = None
val guardHabits = new mutable.HashMap[Int, SleepPeriod]
def guardHabit(id: Int) = guardHabits.getOrElseUpdate(id, new SleepPeriod)

assert(events(0).isInstanceOf[GuardChange])

for (event <- events) {
  event match {
    case GuardChange(id) =>
      assert(lastTime == None)
      guard = id

    case Sleep(startTime) =>
      assert(lastTime == None)
      lastTime = Some(startTime)

    case Wake(endTime) =>
      val duration = endTime - lastTime.get
      assert(duration > 0)
      val habit = guardHabit(guard)
      habit.total += duration
      (lastTime.get until endTime).foreach(habit.byMinute(_) += 1)

      lastTime = None
  }
}

{
  // Method 1: Sleepiest guard's most common minute
  val (sleepiestId, sleepiestHabit) = guardHabits.maxBy(_._2.total)
  val sleepiestMinute = sleepiestHabit.byMinute.zipWithIndex.maxBy(_._1)._2
  println(s"Sleepiest guard $sleepiestId * sleepiest minute $sleepiestMinute = ${sleepiestId*sleepiestMinute}")
}

{
  // Method 2: Best single minute out of entire selection
  val (id, minute, freq) = guardHabits.view.map { case (id, habit) =>
    val (freq, minute) = habit.byMinute.zipWithIndex.maxBy(_._1)
    (id, minute, freq)
  }.maxBy(_._3)
  println(s"Best minute: Guard $id, minute $minute, at $freq times total. Answer: ${id*minute}")
}
