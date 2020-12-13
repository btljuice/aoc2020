import scala.io.Source
import scala.collection.Searching._
import scala.util.control.Breaks._

// TODO: Use a HashMap

val entries =  Source.fromFile("aoc1.txt")
  .getLines
  .map(_.toInt)
  .toVector
  .sorted // O(n^log(n))

implicit class RichVector[T](v: Vector[T])(implicit ord: Ordering[T]) {
  def searchOption(t : T): Option[T] = v.search(t) match { case Found(_) => Some(t); case _ => None }
}

// Part 1
//val answers1 = entries.combinations(2).find(_.sum == 2020).map(_.product) // O(n^2)
//println(answers1.toList)
breakable {
  for { // O(n^log(n))
    t <- entries.tails if t.size >= 2
    i = t.head
    j <- t.tail.searchOption(2020 - i)
        } {
    println(s"Answer 1 = ${i * j}")
    break
  }
}

// Part 2
//val answers2 = entries.combinations(3).find(_.sum == 2020).map(_.product)
//println(answers2.toList)
breakable {
  for { // O(n^2log(n))
    l1 <- entries.tails if l1.size >= 2
    i = l1.head
    l2 <- l1.tails if l2.size >= 2
    j = l2.head
    k <- l2.tail.searchOption(2020 - i - j)
  } {
    println(s"Answer 2 = ${i * j * k}")
    break
  }
}
