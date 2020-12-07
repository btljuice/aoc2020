import scala.io.Source

case class Survey(answers: List[String]) {
  def total = answers.map(_.toSet).reduce(_ union _).size
  def intersection = answers.map(_.toSet).reduce(_ intersect _).size
}

val surveys = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc6.txt")
  .getLines
  .toStream
  .append(Stream(""))
  .foldLeft((Vector.empty[Survey], List.empty[String])) { case ((surveys, cur), line) =>
    if (line.isEmpty) {
      (surveys :+ Survey(cur), Nil)
    } else {
      (surveys, line :: cur)
    }
  }._1

val answer1: Int = surveys.map(_.total).sum
val answer2: Int = surveys.map(_.intersection).sum

