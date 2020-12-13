import scala.io.Source

val entries = Source.fromFile("aoc3.txt")
  .getLines
  .zipWithIndex
  .toStream

def descendForest(right: Int, down: Int): Long =
  entries
    .drop(1) // First line is never evaluated
    .count { case (str, i) =>
      lazy val j = i/down*right % str.length
      i % down == 0 && str(j) == '#'
    }

val answer1 = descendForest(3, 1)

val answer2 =
  descendForest(1, 1) *
  descendForest(3, 1) *
  descendForest(5, 1) *
  descendForest(7, 1) *
  descendForest(1, 2)
