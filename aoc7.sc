import scala.annotation.tailrec
import scala.io.Source

val ruleRegex = raw"(.+) bags contain (.+)".r
val containedRegex = raw"\d+ (\D+) bag[s]?[,.]".r

val contains = Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc7.txt")
  .getLines
  .flatMap { case ruleRegex(bag, containedBagsStr) =>
    val containedBags = containedRegex.findAllMatchIn(containedBagsStr).map(_.group(1)).toSet
    if (containedBags.nonEmpty) Some(bag -> containedBags) else None
  }.toMap

val containedBy = contains
  .map { case (k, v) => v.map(_ -> k) }.flatten
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

val answer1 = bfs(containedBy, "shiny gold", 0)( (_, i) => i + 1) - 1

