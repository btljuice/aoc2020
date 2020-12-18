import scala.io.Source
import breeze.linalg._
import breeze.numerics._

def rotMat(action: String) = action match {
  case "R90" | "L270" => DenseMatrix( (0,  1), (-1, 0) )
  case "L90" | "R270" => DenseMatrix( (0, -1), ( 1, 0) )
  case "R180" | "L180" => diag(DenseVector(-1, -1))
  case r => sys.error(s"Unknown rotation $r")
}

def displacement(dir: Char, d: Int): DenseVector[Int] = dir match {
  case 'N' => DenseVector( 0,  d)
  case 'S' => DenseVector( 0, -d)
  case 'E' => DenseVector( d,  0)
  case 'W' => DenseVector(-d,  0)
  case c => sys.error(s"Unknown direction: $c")
}

val actions = Source.fromFile("aoc12.txt")
  .getLines
  .toStream

val answer1 = actions.foldLeft { (DenseVector(1, 0), DenseVector(0, 0)) } { case ((dir, xy), action) =>
//  println(s"dir = $dir, xy = $xy")
  lazy val d = action.tail.toInt
  action.head match {
    case 'F' => (dir, xy + d*dir)
    case 'R' | 'L' => (rotMat(action)*dir, xy)
    case 'N' | 'E' | 'S' | 'W' => (dir, xy + displacement(action.head, d))
    case c => sys.error(s"Unknown character $c")
  }
}._2.map(_.abs).sum


val answer2 = actions.foldLeft{ (DenseVector(10, 1), DenseVector(0, 0)) } { case ((waypoint, xy), action) =>
  //  println(s"waypoint = $waypoint, xy = $xy")
  lazy val d = action.tail.toInt
  action.head match {
    case 'F' => (waypoint, xy + d*waypoint)
    case 'R' | 'L' => (rotMat(action)*waypoint, xy)
    case 'N' | 'E' | 'S' | 'W' => (waypoint + displacement(action.head, d), xy)
    case c => sys.error(s"Unknown character $c")
  }
}._2.map(_.abs).sum

