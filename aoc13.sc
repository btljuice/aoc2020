import scala.io.Source

val file = Source.fromFile("aoc13.txt").getLines

val timestamp = file.next.toLong
val buses: IndexedSeq[Option[Long]] = file.next.split(',').map { case "x" => None; case s => Some(s.toLong) }

// Earliest bus
val answer1 = buses.flatten.toSet.map { id: Long =>
    val timeToWait = if (timestamp % id == 0) 0 else (timestamp/id + 1)*id - timestamp
    id -> timeToWait
  }.minBy(_._2) match { case (id, timeToWait) => id*timeToWait }


val answer2 = {
  val validBuses = buses
    .zipWithIndex
    .collect { case (Some(id), i) => (id, i) }
    .sortBy(-_._1)

  val (offset0, step0) = validBuses.head match { case (id, i) => (id - i, id) }

  validBuses.tail.foldLeft { (offset0, step0) } { case ((offset, step), (id, i)) =>
    var n = offset
    while ((n + i) % id != 0) n += step
    (n, step*id)
  }
}
