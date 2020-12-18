import scala.io.Source

def rotate(dir: Char, degrees: Int) = (dir, degrees) match {
  case ('R',  90) | ('L', 270) => Map('N'->'E', 'E'->'S', 'S'->'W', 'W'->'N')
  case ('L',  90) | ('R', 270) => Map('N'->'W', 'W'->'S', 'S'->'E', 'E'->'N')
  case ('R', 180) | ('L', 180) => Map('N'->'S', 'E'->'W', 'S'->'N', 'W'->'E')
}

def displace(dir: Char, d: Int, xy: (Int, Int)): (Int, Int) = xy match { case (x, y) => dir match {
  case 'N' => (x,     y + d)
  case 'S' => (x,     y - d)
  case 'E' => (x + d, y    )
  case 'W' => (x - d, y    )
  case c => sys.error(s"Unknown direction: $c")
} }

val actions = Source.fromFile("aoc12.txt")
  .getLines
  .map { l => (l.head, l.tail.toInt) }
  .toStream

val answer1 = actions.foldLeft(('E', (0, 0))) { case ((dir, xy), (action, d)) =>
  action match {
    case 'F' => (dir, displace(dir, d, xy))
    case 'R' | 'L' => (rotate(action, d)(dir), xy)
    case 'N' | 'E' | 'S' | 'W' => (dir, displace(action, d, xy))
    case c => sys.error(s"Unknown character $c")
  }
}._2

