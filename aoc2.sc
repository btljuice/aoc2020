import scala.io.Source

val regex = raw"(\d+)-(\d+) (.): (.*)".r
val entries = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc2.txt")
  .getLines
  .collect { case regex(min, max, c, password) => (min.toInt, max.toInt, c.head, password) }
  .toStream

val answer1 =  entries
  .count { case (min, max, c, password) =>
    val nb_c = password.count(_ == c)
    min <= nb_c && nb_c <= max
  }

val answer2 = entries
  .count { case (n1, n2, c, password) =>
    val match1 = if (password(n1 - 1) == c) 1 else 0
    val match2 = if (password(n2 - 1) == c) 1 else 0
    match1 + match2 == 1
  }
