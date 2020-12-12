import scala.io.Source
import scala.collection.mutable

def getNumbersIte = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc9.txt")
  .getLines
  .map(_.toInt)


def findFirstWeakness(windowSize: Int): Option[Int] = {
  val numbersIte = getNumbersIte

  val solutions = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set.empty[Int]) // solution -> numbers
  val numbers = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set.empty[Int]) // number -> solutions
  val lastNumbers = mutable.Queue.empty[Int]

  def enqueueNumber(n: Int): Unit = {
    for { m <- lastNumbers ; if n != m; s = n + m } {
      solutions(s) = solutions(s) + n + m
      numbers(n) = numbers(n) + s
      numbers(m) = numbers(m) + s
    }
    lastNumbers.enqueue(n)
  }

  def dequeueNumber(): Unit = {
    val n = lastNumbers.dequeue()
    val dequeuedSolutions = numbers.remove(n).get
    for { s <- dequeuedSolutions } {
      val newSolutions = solutions(s) - n
      if (newSolutions.isEmpty) {
        solutions.remove(s)
      } else {
        solutions(s) = newSolutions
      }
    }
  }

  // 1. Initialization. Fills up lastNumbers queue to windowSize
  for { n <- numbersIte.take(windowSize) } { enqueueNumber(n) }

  // 2. Rolls through the rest of the numbers
  numbersIte.find { n =>
    require(!numbers.contains(n), "Assumption failed: lastNumber queue cannot contain same number")
    if (!solutions.contains(n)) {
      true
    } else {
      // Dequeue one number
      dequeueNumber()
      // Enqueue new number
      enqueueNumber(n)
      false
    }
  }
}

val answers1 = findFirstWeakness(25)
