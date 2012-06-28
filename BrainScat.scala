object Main {

  def main(args: Array[String]) {
    val rawInput = " +++these+++ are++++[>+++comments++++>+++in+++a++++>+++brainfuck>+<<<<-]program!!!>++.>+.++++lol+++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
    val bf = BrainScat(rawInput)
    bf.eval
  }
}

class BrainScat (program: String) {
  val tape = new Tape(program)
  val data = new DataRegister

  def eval {
    while (tape.canMoveRight) {
      tape.moveRight
      val symbol = tape.get
      val reg = data.get
      symbol match {
          case '>' => data.shiftRight
          case '<' => data.shiftLeft
          case '+' => data.increment
          case '-' => data.decrement
          case '.' => data.out
          case ',' => data.in
          case '[' if data.get == 0 => tape.jumpForward
          case ']' if data.get != 0 => tape.jumpBackward
          case _ => 
      }
    }
    println("\nReached end of input; Exiting.")
  }
}

object BrainScat {

  val validChars = "+-[]<>,."

  def apply(program: String) = {
    new BrainScat(sanitize(program));
  }

  def sanitize(input: String) = { 
    input.filter((char: Char) => validChars.contains(char))
  }
}

class Tape (program: String) {

  var pointerIndex = -1

  def get = {
    pointerIndex match {
      case x if x < 0 => throw new Exception("Cannot get symbol; Below tape head")
      case x if x >= program.size => throw new Exception("Cannot get symbol; Past tape tail")
      case _  => program.apply(pointerIndex)
    }
  }

  def canMoveRight = {
    val nextIndex = pointerIndex + 1
    nextIndex < program.size
  }

  def moveRight {
    if ( pointerIndex < program.length - 1) pointerIndex = pointerIndex + 1
    else throw new Exception("Cannot increment pointer; at end of tape.")
  }

  def moveLeft {
    if (pointerIndex > 0) pointerIndex = pointerIndex - 1
    else throw new Exception("Cannot decrement pointer; at beginning of tape.")
  }
  
  def canMoveLeft = {
    val prevIndex = pointerIndex - 1;
    prevIndex >= 0
  }

  def jumpForward {
	var psuedoStack = 1
    while(canMoveRight && psuedoStack != 0) {
      moveRight
      get match {
      	case '[' => psuedoStack = psuedoStack + 1
      	case ']' => psuedoStack = psuedoStack - 1
      	case _ =>
      }
    }
  }

  def jumpBackward {
    var psuedoStack = 1
    while(canMoveLeft && psuedoStack != 0) {
      moveLeft
      get match {
      	case ']' => psuedoStack = psuedoStack + 1
      	case '[' => psuedoStack = psuedoStack - 1
      	case _ =>
      }
    }
  }
}

class DataRegister {

  val data = new Array[Int](3000)
  var pointerIndex = 0

  def get = {
    data.apply(pointerIndex)
  }

  def increment {
    data.update(pointerIndex, get + 1)    
  }

  def decrement {
    if (get > 0) data.update(pointerIndex, get - 1)    
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
    print(get.toChar)
  }

  def in {
    //TODO?
  }
}

