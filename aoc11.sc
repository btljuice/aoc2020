import scala.annotation.tailrec
import scala.collection.mutable
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

implicit class RichSeats(val seats: IndexedSeq[IndexedSeq[Space]]) {
  def printSeats: Unit = {
    seats.foreach { l =>
      val lineStr = l.map { case Floor => '.'; case Empty => 'L'; case Occupied => '#'; }.mkString
      println(lineStr)
    }
    println("")
  }

  def getSpace(i: Int, j: Int): Option[Space] = seats.lift(i).flatMap(_.lift(j))
}

def occupy(
  visibleLookup: (Int, Int) => Seq[(Int, Int)],
  rule: (Space, => Int) => Space,
): IndexedSeq[IndexedSeq[Space]] = {
  @tailrec def occupyImpl(seats: IndexedSeq[IndexedSeq[Space]], count: Int = 0): IndexedSeq[IndexedSeq[Space]] = {
    def isOccupied(i: Int, j: Int): Boolean = seats.getSpace(i, j).contains(Occupied)
    def countOccupied(visibleIndices: Seq[(Int, Int)]): Int = visibleIndices.count { case (i, j) => isOccupied(i, j) }

    var modified = false
    val newSeats = {
      for { i <- seats.indices } yield {
        for { j <- seats(i).indices } yield {
          val s = seats(i)(j)
          lazy val nbOccupied = countOccupied(visibleLookup(i, j))
          val ns = rule(s, nbOccupied)
          if (s != ns) modified = true
          ns
        }
      }
    }
    newSeats.printSeats
    if (modified /* && count <= 5*/) occupyImpl(newSeats, count + 1) else newSeats
  }

  occupyImpl(initialSeats)
}

val answer1 = {
  def adjacents(i: Int, j: Int): Seq[(Int, Int)] =
    for { di <- -1 to 1; dj <- -1 to 1; if di != 0 || dj != 0 } yield { (i + di, j + dj) }

  def rules(s: Space, nbOccupied: => Int): Space = s match {
    case Floor => Floor
    case Empty if nbOccupied == 0 => Occupied
    case Occupied if nbOccupied >= 4 => Empty
    case x => x
  }

  occupy(adjacents, rules).flatten.count { case Occupied => true; case _ => false }
}

// Answer 2
def memoize2[I, J, O](f: (I, J) => O): (I, J) => O = {
  val mem = new mutable.HashMap[(I, J), O]
  (i,j) => mem.getOrElseUpdate((i, j), f(i, j))
}

val answer2 = {
  val diagonals: (Int, Int) => Seq[(Int, Int)] = memoize2 { (i, j) =>
    @tailrec def visible(i: Int, j: Int, di: Int, dj: Int): Option[(Int, Int)] =
      initialSeats.getSpace(i, j) match {
        case Some(Floor) => visible(i + di, j + dj, di, dj)
        case Some(Empty | Occupied) => Some((i, j))
        case None => None
      }

      for {
        di <- -1 to 1; dj <- -1 to 1; if di != 0 || dj != 0
        ij <- visible(i + di, j + dj, di, dj)
      } yield { ij }
  }


  def rules(s: Space, nbOccupied: => Int): Space = s match {
    case Floor => Floor
    case Empty if nbOccupied == 0 => Occupied
    case Occupied if nbOccupied >= 5 => Empty
    case x => x
  }


  occupy(diagonals, rules).flatten.count { case Occupied => true; case _ => false }
}
