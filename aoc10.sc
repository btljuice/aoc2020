import scala.io.Source
import scala.collection.mutable

// Adapter output
val adapters = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc10.txt")
  .getLines
  .map(_.toInt)
  .toArray
  .sorted

// Adapter input can only be 1, 2, or 3 jolt lower
// Seat's outlet is 0
// Device input is Adapters max + 3

val seatOutlet = 0
val maxDeviceJolt = adapters.last + 3

def differences = (seatOutlet #:: adapters.toStream #::: Stream(maxDeviceJolt))
  .sliding(2)
  .map { case Stream(i, j) => j - i }

val answer1 = differences.count(_ == 1) * differences.count(_ == 3)

// Answer 2
def memoize[I, O](f: I => O): I => O = {
  val mem = new mutable.HashMap[I, O]
  i => mem.getOrElseUpdate(i, f(i))
}

// Starts w/ the last adapter (as it is the only one connecting to the device) and works toward the seat input
val adaptersSet = adapters.toSet
lazy val nbArrangements: Int => Long = memoize { j =>
  if (j < 0) 0 // This arrangement can't connect to the seat
  else if (j == 0) 1 // This arrangement connects w/ the exact voltage
  else if (!adaptersSet(j)) 0 // Cannot find the adapter
  else {
    nbArrangements(j - 1) + nbArrangements(j - 2) + nbArrangements(j - 3)
  }
}

val answer2 = nbArrangements(adaptersSet.max)

