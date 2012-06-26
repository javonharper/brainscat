object Main {

  def main(args: Array[String]) {
    /*val rawInput = "thisisabrainfuck%%%%interpreterlololol>+++++++6+6+8[9<0+3+2skldfjalksdf+2ldjfald+5ldkfjdl+6+8+9+4>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<++++++++>-]<-.--------.+++>.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."*/
    /*val bf = BrainFuck(rawInput)*/
    /*bf eval*/
  }
}

class BrainFuck (program: String) {
  val tape = new Tape
  val data = new DataRegister

  def eval {
    //DUMMY MATCH
    'X' match {
        case '>' => tape.increment
        case '<' => tape.decrement
        case '+' => data.increment
        case '-' => data.decrement
        case '.' => data.out
        case ',' => data.in
        case '[' if data.get == 0 => tape.jumpForward
        case ']' if data.get != 0 => tape.jumpBackward
        case _ => 
    }
  }
}

object BrainFuck {

  val validChars = "+-[]<>,."

  def apply(program: String) = {
    new BrainFuck(sanitize(program));
  }

  def sanitize(input: String) = { 
    input.filter((char: Char) => validChars.contains(char))
  }
}

class Tape {
  
  def increment {
      
  }

  def decrement {
      
  }

  def jumpForward {
      
  }

  def jumpBackward {
      
  }
}

class DataRegister {

  def get = {
    //TODO implement me
     0 
  }

  def increment {
      
  }

  def decrement {
      
  }

  def in {
      
  }

  def out {
      
  }
}
