import scala.io.Source

case class Survey(
  answers: String,
  nbRespondents: Int,
) {
  def add(newAnswers: String) = Survey(answers + newAnswers, nbRespondents + 1)
}
object Survey {
  def empty = Survey("", 0)
}

val surveys = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc6.txt")
  .getLines
  .toStream
  .append(Stream(""))
  .foldLeft((Vector.empty[Survey], Survey.empty)) { case ((surveys, cur), line) =>
    if (line.isEmpty) {
      (surveys :+ cur, Survey.empty)
    } else {
      (surveys, cur.add(line))
    }
  }._1

val answer1: Int = surveys.map(_.answers.groupBy(c => c).size).sum
val answer2: Int = surveys.map{ s =>
  s.answers
    .groupBy(c => c)
    .mapValues(_.size)
    .count { case (_, v) => v == s.nbRespondents }
}.sum

