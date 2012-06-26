object Main {
  def main(args: Array[String]) {
    val rawInput = "thisisabrainfuck%%%%interpreterlololol>+++++++6+6+8[9<0+3+2skldfjalksdf+2ldjfald+5ldkfjdl+6+8+9+4>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<++++++++>-]<-.--------.+++>.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
    new Interpreter(rawInput)
  }
}

class Interpreter (rawInput: String) {
  val program = sanitize(rawInput)
  val validChars = "<>[]+-.,"
  def sanitize(input: String) = { input.filter((char: Char) => validChars.contains(char)) }
}
