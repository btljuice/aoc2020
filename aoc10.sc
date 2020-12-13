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
val maxDeviceJolt = adapters.max + 3

def differences = (seatOutlet #:: adapters.toStream #::: Stream(maxDeviceJolt))
  .sliding(2)
  .map { case Stream(i, j) => j - i }

val answer1 = differences.count(_ == 1) * differences.count(_ == 3)
