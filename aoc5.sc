import scala.io.Source

val entries = Source.fromFile("aoc5.txt")
  .getLines
  .toStream
  .map { _.map {
    case 'F' | 'L' => '0'
    case 'B' | 'R' => '1'
  } }
  .map(Integer.parseInt(_, 2))

val answer1 = entries.max
val answer2 = entries
  .sorted
  .sliding(2)
  .collectFirst { case Stream(cur, nxt) if nxt - cur > 1 => cur + 1 }
