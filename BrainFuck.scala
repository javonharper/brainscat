object Main {

  def main(args: Array[String]) {
    val rawInput = "thisisabrainfuck%%%%interpreterlololol>+++++++6+6+8[9<0+3+2skldfjalksdf+2ldjfald+5ldkfjdl+6+8+9+4>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<++++++++>-]<-.--------.+++>.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
    val bf = BrainFuck(rawInput)
  }
}

class BrainFuck (program: String) {
}

object BrainFuck {

  val validChars = "+-[]<>,."

  def apply(program: String) {
    new BrainFuck(sanitize(program));
  }

  def sanitize(input: String) = { 
    input.filter((char: Char) => validChars.contains(char))
  }
}
