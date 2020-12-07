import scala.collection.mutable
import scala.io.Source

case class Survey(
  answersFreq: mutable.Map[Char, Int],
  nbRespondents: Int,
)
object Survey {
  def empty = Survey(mutable.Map.empty[Char, Int], 0)
}

val surveys = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc6.txt")
  .getLines
  .toStream
  .append(Stream(""))
  .foldLeft((Vector.empty[Survey], Survey.empty)) { case ((surveys, cur), line) =>
    if (line.isEmpty) {
      (surveys :+ cur, Survey.empty)
    } else {
      line.foreach { c => cur.answersFreq(c) = cur.answersFreq.getOrElse(c, 0) + 1 }
      (surveys, Survey(cur.answersFreq, cur.nbRespondents + 1))
    }
  }._1

val answer1: Int = surveys.map(_.answersFreq.size).sum
val answer2: Int = surveys.map(s =>
  s.answersFreq.count { case (_, v) => v == s.nbRespondents }
).sum

