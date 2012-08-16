object Main {

  def main(args: Array[String]) {
    val rawInput = " +++these+++ are++++[>+++comments++++>+++in+++a++++>+++brainfuck>+<<<<-]program!!!>++.>+.++++lol+++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
    val bf = Brainscat(rawInput)
    bf.eval
  }
}

class Brainscat (symbols: String) {
  val program = new Program(symbols)
  val data = new DataRegister

  def eval {
    while (program.canMoveRight) {
      program.moveRight()
      val symbol = program.get
      symbol match {
          case '>' => data.shiftRight()
          case '<' => data.shiftLeft()
          case '+' => data.increment()
          case '-' => data.decrement()
          case '.' => data.out
          case ',' => data.in
          case '[' if data.get == 0 => program.jumpForward()
          case ']' if data.get != 0 => program.jumpBackward()
          case _ => 
      }
    }
    println("\nReached end of input; Exiting.")
  }
}

object Brainscat {

  val validChars = "+-[]<>,."

  def apply(program: String) = {
    new Brainscat(sanitize(program));
  }

  def sanitize(input: String) = { 
    input.filter((char: Char) => validChars.contains(char))
  }
}

class Program (program: String) {

  var pointerIndex = -1

  def get = {
    pointerIndex match {
      case x if x < 0 => throw new Exception("Cannot get symbol; At position before program beginning")
      case x if x >= program.size => throw new Exception("Cannot get symbol; At position before program end")
      case _  => program(pointerIndex)
    }
  }

  def canMoveRight = {
    val nextIndex = pointerIndex + 1
    nextIndex < program.size
  }

  def moveRight() {
    if ( pointerIndex < program.length - 1) pointerIndex = pointerIndex + 1
    else throw new Exception("Cannot increment pointer; at end of program.")
  }

  def moveLeft() {
    if (pointerIndex > 0) pointerIndex = pointerIndex - 1
    else throw new Exception("Cannot decrement pointer; at beginning of program.")
  }
  
  def canMoveLeft = {
    val prevIndex = pointerIndex - 1;
    prevIndex >= 0
  }

  def jumpForward() {
	var psuedoStack = 1
    while(canMoveRight && psuedoStack != 0) {
      moveRight()
      get match {
      	case '[' => psuedoStack = psuedoStack + 1
      	case ']' => psuedoStack = psuedoStack - 1
      	case _ =>
      }
    }
  }

  def jumpBackward() {
    var psuedoStack = 1
    while(canMoveLeft && psuedoStack != 0) {
      moveLeft()
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
    data(pointerIndex)
  }

  def increment() {
    data.update(pointerIndex, get + 1)    
  }

  def decrement() {
    if (get > 0) data.update(pointerIndex, get - 1)    
  }

  def shiftRight() {
    if (pointerIndex < data.size) pointerIndex = pointerIndex + 1
    else throw new Exception("Cannot increment pointer; at end of data register.")
  }

  def shiftLeft() {
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

