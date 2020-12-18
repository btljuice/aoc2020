import scala.io.Source

val file = Source.fromFile("aoc13.txt").getLines

val timestamp = file.next.toLong
val buses: IndexedSeq[Option[Long]] = file.next.split(',').map { case "x" => None; case s => Some(s.toInt) }

// Earliest bus
val answer1 = buses.flatten.toSet.map { id: Long =>
    val timeToWait = if (timestamp % id == 0) 0 else (timestamp/id + 1)*id - timestamp
    id -> timeToWait
  }.minBy(_._2) match { case (id, timeToWait) => id*timeToWait }


// Brute force w/ the highest value
val answer2 = {
  val validBuses = buses.zipWithIndex.collect { case (Some(id), i) => (id, i) }
  def isSolution(n: Long) = validBuses.forall { case (id, i) => (n + i) % id == 0 }

  val (maxId, maxI) = validBuses.maxBy(_._1)
  var t = maxId - maxI
  while (!isSolution(t)) t += maxId

  t
}
