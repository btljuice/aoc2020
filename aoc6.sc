import scala.io.Source

case class Survey(
  answers: Map[Char, Int],
  nbRespondents: Int,
)
object Survey {
  def apply(answers: List[String]): Survey = {
    Survey(
      answers.flatMap(_.toCharArray).groupBy(c => c).mapValues(_.size),
      answers.size,
    )
  }
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

val answer1: Int = surveys.map(_.answers.size).sum
val answer2: Int = surveys.map{ s => s.answers.count(_._2 == s.nbRespondents) }.sum

