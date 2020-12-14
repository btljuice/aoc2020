import scala.annotation.tailrec
import scala.io.Source

sealed trait Space
sealed trait Seat extends Space
final object Empty extends Seat
final object Occupied extends Seat
final object Floor extends Space

val initialSeats: IndexedSeq[IndexedSeq[Space]] = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc11.txt")
  .getLines
  .map( _.map {
    case '.' => Floor
    case 'L' => Empty
    case _ => sys.error("Unknown Space Type")
  }.toIndexedSeq)
  .toIndexedSeq

def printSeats(seats: IndexedSeq[IndexedSeq[Space]]): Unit = {
  seats.foreach { l =>
    val lineStr = l.map { case Floor => '.'; case Empty => 'L'; case Occupied => '#'; }.mkString
    println(lineStr)
  }
  println("")
}

@tailrec def occupy(seats: IndexedSeq[IndexedSeq[Space]], count: Int = 0): IndexedSeq[IndexedSeq[Space]] = {
  def occupied(i: Int, j: Int) = if (seats.lift(i).flatMap(_.lift(j)).contains(Occupied)) 1 else 0
  def countOccupied(i: Int, j: Int): Int =
    ( for { di <- -1 to 1; dj <- -1 to 1; if di != 0 || dj != 0 } yield { occupied(i + di, j + dj)} ).sum

  var modified = false
  val newSeats = {
    for { i <- seats.indices } yield {
      for { j <- seats(i).indices } yield {
        val s = seats(i)(j)
        lazy val nbOccupied = countOccupied(i, j)
        val ns = s match {
          case Floor => Floor
          case Empty if nbOccupied == 0 => modified = true; Occupied
          case Occupied if nbOccupied >= 4 => modified = true; Empty
          case x => x
        }
        ns
      }
    }
  }
  //    printSeats(newSeats)
  if (modified /* && count <= 5*/) occupy(newSeats, count + 1) else newSeats
}


val answer1 = {
  @tailrec def occupy(seats: IndexedSeq[IndexedSeq[Space]], count: Int = 0): IndexedSeq[IndexedSeq[Space]] = {
    def occupied(i: Int, j: Int) = if (seats.lift(i).flatMap(_.lift(j)).contains(Occupied)) 1 else 0
    def countOccupied(i: Int, j: Int): Int =
      ( for { di <- -1 to 1; dj <- -1 to 1; if di != 0 || dj != 0 } yield { occupied(i + di, j + dj)} ).sum

    var modified = false
    val newSeats = {
      for { i <- seats.indices } yield {
        for { j <- seats(i).indices } yield {
          val s = seats(i)(j)
          lazy val nbOccupied = countOccupied(i, j)
          val ns = s match {
            case Floor => Floor
            case Empty if nbOccupied == 0 => modified = true; Occupied
            case Occupied if nbOccupied >= 4 => modified = true; Empty
            case x => x
          }
          ns
        }
      }
    }
//    printSeats(newSeats)
    if (modified /* && count <= 5*/) occupy(newSeats, count + 1) else newSeats
  }

  occupy(initialSeats).flatten.count { case Occupied => true; case _ => false }
}
