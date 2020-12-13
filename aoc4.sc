import scala.io.Source
import scala.util.Try

val entries = Source.fromFile("aoc4.txt")
  .getLines
  .toStream

val validFields = Set[String]("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", /*"cid"*/)

val lineRegex = raw"(\S+):(\S+)".r
val heightRegex = raw"(\d+)(cm|in)".r
val colorRegex = raw"#[0-9a-f]{6}"
val passportIdRegex = raw"\d{9}"

def isPassportValid1(fields: Map[String, String]) = validFields subsetOf fields.keySet

def isPassportValid2(fields: Map[String, String]) = {
  val byr = Try { 1920 to 2002 contains fields("byr").toInt } getOrElse false
  lazy val iyr = Try { 2010 to 2020 contains fields("iyr").toInt } getOrElse false
  lazy val eyr = Try { 2020 to 2030 contains fields("eyr").toInt } getOrElse false
  lazy val hgt = Try { fields("hgt") match {
    case heightRegex(cms, "cm") => 150 to 193 contains cms.toInt
    case heightRegex(inches, "in") => 59 to 76 contains inches.toInt
  } } getOrElse false
  lazy val hcl = fields.get("hcl").exists { _.matches(colorRegex) }
  lazy val ecl = fields.get("ecl").exists {
    case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
    case _ => false
  }
  lazy val pid =  fields.get("pid").exists { _.matches(passportIdRegex) }

  byr && iyr & eyr && hgt && hcl && ecl && pid
}

def countPassports(isValid: Map[String, String] => Boolean): Int =
  entries
    .append("" :: Nil) // Artificially insert a NL ensuring the last passport is processed.
    .foldLeft( (0, Map.empty[String, String]) ) { case ((nb_valid, passport), line) =>
      if (line.isEmpty) {
        val validInc = if (isValid(passport)) 1 else 0
        (nb_valid + validInc, Map.empty[String, String])
      } else {
        val newFields = lineRegex.findAllMatchIn(line).map { m => m.group(1) -> m.group(2) }
        (nb_valid, passport ++ newFields)
      }
    }._1

val answer1 = countPassports(isPassportValid1)
val answer2 = countPassports(isPassportValid2)




