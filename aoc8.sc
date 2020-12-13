import scala.io.Source


case class Instruction(label: String, counter: Int)

val instructionRegex = raw"(nop|acc|jmp) ([+-]\d+)".r

val code = Source.fromFile("aoc8.txt")
  .getLines
  .map { case instructionRegex(label, counter) => Instruction(label, counter.toInt) }
  .toVector

class Code {
  def apply(i : Int): Instruction = code(i)
  def size: Int = code.size
}

case class PatchedCode(patched: Instruction, lineNo: Int) extends Code {
  override def apply(i : Int): Instruction = if (i == lineNo) patched else code(i)
}

def run(code: Code): (Int, Int, Boolean) = {
  var acc = 0
  var lineNo = 0
  val visited = Array.fill[Boolean](code.size)(false)

  while (0 <= lineNo && lineNo < code.size && !visited(lineNo)) {
    visited(lineNo) = true
    code(lineNo) match {
      case Instruction("nop", _) => lineNo += 1
      case Instruction("acc", n) => lineNo += 1; acc += n
      case Instruction("jmp", n) => lineNo += n
    }
  }
  (acc, lineNo, visited.forall(_ == true))
}

val answer1 = run(new Code)._1

val answer2 = code
  .toStream
  .zipWithIndex
  .collect {
    case (Instruction("nop", n), lineNo) => PatchedCode(Instruction("jmp", n), lineNo)
    case (Instruction("jmp", n), lineNo) => PatchedCode(Instruction("nop", n), lineNo)
  }.map { run(_) }
  .collectFirst { case (acc, lineNo, _) if lineNo == code.size => acc }
