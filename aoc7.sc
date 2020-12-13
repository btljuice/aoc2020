import scala.annotation.tailrec
import scala.io.Source

val ruleRegex = raw"(.+) bags contain (.+)".r
val containedRegex = raw"(\d+) (\D+) bag[s]?[,.]".r

case class BagInfo(n: Int, color: String)

val contains = Source.fromFile("aoc7.txt")
  .getLines
  .flatMap { case ruleRegex(bag, containedBagsStr) =>
    val containedBags = containedRegex.findAllMatchIn(containedBagsStr).map { m =>
      BagInfo(m.group(1).toInt, m.group(2))
    }.toList
    if (containedBags.nonEmpty) Some(bag -> containedBags) else None
  }.toMap

val containedBy = contains
  .map { case (k, v) => v.map(_.color -> k) }.flatten
  .groupBy(_._1)
  .mapValues(_.map(_._2).toSet)

def bfs[T](tree: Map[String, Set[String]], from: String, startAcc: T)(f: (String, T) => T) = {
  @tailrec def bfsImpl(toSearch: Set[String], searched: Set[String], acc: T): T =
    if (toSearch.isEmpty) acc
    else {
      val cur = toSearch.head
      val newAcc = f(cur, acc)
      val newEntries = tree.getOrElse(cur, Set.empty) - cur -- searched
      bfsImpl(toSearch - cur ++ newEntries, searched + cur, newAcc)
    }

  bfsImpl(Set(from), Set.empty, startAcc)
}

val answer1 = bfs(containedBy, "shiny gold", 0)( (_, i) => i + 1) - 1 // - 1 because we don't count "shiny gold"

val answer2 = {
  def countBags(color: String): Int = {
    1 +
      contains.getOrElse(color, Nil)
        .map { case BagInfo(n, color) => n*countBags(color) }.sum
  }
  countBags("shiny gold") - 1 // -1 as we don't count the "shiny gold" bag
}

