import java.lang.Long.parseLong
import scala.io.Source

// Long 64 signed-bit is used for convenience as unsigned types are not readily available in scala.

val maskRegex = raw"mask = ([X01]{36})".r
val memRegex = raw"mem\[(\d+)\] = (\d+)".r

sealed trait Instruction
final case class SetMask(andMask: Long, orMask: Long) extends Instruction
final case class SetMemory(address: Long, value: Long) extends Instruction

case class State(andMask: Long, orMask: Long, mem: Map[Long, Long])
object State { val init = State(0xFFFFFFFFFL, 0x0, Map.empty) }

// Bitmask: 36bit little-endian
val instructions =
  Source.fromFile("/Users/atrudeau/code-non-hopper/aoc2020/input/aoc14.txt")
    .getLines
    .toStream
    .map {
      case maskRegex(mask) =>
        SetMask(
          andMask = parseLong(mask.replace('X', '1'), 2),
          orMask  = parseLong(mask.replace('X', '0'), 2),
        )
      case memRegex(address, value) =>
        SetMemory(address.toInt, value.toInt)
    }

val answer1 = instructions.foldLeft(State.init) {
  case (s, SetMask(andMask, orMask)) => State(andMask, orMask, s.mem)
  case (s, SetMemory(address, value)) =>
    val maskedValue = value & s.andMask | s.orMask
    val newKv = address -> maskedValue
    State(s.andMask, s.orMask, s.mem + newKv)
}.mem.values.sum

