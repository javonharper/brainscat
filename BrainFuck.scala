import java.util.Scanner

object Main {

  def main(args: Array[String]) {
    val rawInput = "thisisabrainfuck%%%%interpreterlololol>+++++++6+6+8[9<0+3+2skldfjalksdf+2ldjfald+5ldkfjdl+6+8+9+4>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<++++++++>-]<-.--------.+++>.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
    val bf = BrainFuck(rawInput)
    bf eval
  }
}

class BrainFuck (program: String) {
  val tape = new Tape(program)
  val data = new DataRegister

  def eval {
    while (tape.canMoveRight) {
      tape.moveRight
      tape.get
      /*println(tape.get)*/
    //DUMMY MATCH
    /*'X' match {*/
    /*    case '>' => data.shiftRight*/
    /*    case '<' => data.shiftLeft*/
    /*    case '+' => data.increment*/
    /*    case '-' => data.decrement*/
    /*    case '.' => data.out*/
    /*    case ',' => data.in*/
    /*    case '[' if data.get == 0 => tape.jumpForward*/
    /*    case ']' if data.get != 0 => tape.jumpBackward*/
    /*    case _ => */
    /*}*/
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

class Tape (program: String) {

  var pointerIndex = 0

  def get = {
    println("size:" + program.size + ", index:" + pointerIndex)
    pointerIndex match {
      case x if x < 0 => throw new Exception("Cannot get symbol; Below tape head")
      case x if x >= program.size => throw new Exception("Cannot get symbol; Past tape tail")
      case _  => program.apply(pointerIndex)
    }
  }

  def canMoveRight = {
    pointerIndex >= 0 && pointerIndex < program.size - 1
  }

  def moveRight {
    if ( pointerIndex < program.length - 1) pointerIndex = pointerIndex + 1
    else throw new Exception("Cannot increment pointer; at end of tape.")
  }

  def moveLeft {
    if (pointerIndex > 0) pointerIndex = pointerIndex - 1
    else throw new Exception("Cannot decrement pointer; at beginning of tape.")
  }

  def jumpForward {
      
  }

  def jumpBackward {
      
  }
}

class DataRegister {

  val data = new Array[Int](3000)
  var pointerIndex = 0

  def get = {
    data.apply(pointerIndex)
  }

  def set (char: Char){
    data.update(pointerIndex, char.toInt)
  }

  def increment {
    data.update(pointerIndex, get + 1)    
  }

  def decrement {
    if (get > 0)
      data.update(pointerIndex, get - 1)    
  }

  def shiftRight {
    if (pointerIndex < data.size) pointerIndex = pointerIndex + 1
    else throw new Exception("Cannot increment pointer; at end of data register.")
  }

  def shiftLeft {
    if (pointerIndex > 0) pointerIndex = pointerIndex - 1
    else throw new Exception("Cannot decrement pointer; at beginning of data register.")
  }

  def out {
    print(" ");
    print(get)
  }

  def in {
    //TODO?
  }
}
