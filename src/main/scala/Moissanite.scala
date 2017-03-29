/**
  * Created by dr0l3 on 3/29/17.
  */
object Moissanite {
  type ErrorMessage = String
  def alphabet = "ABCDEFGHIJLKMNOPQRSTUVWYXZÆØÅ"
  def newline: String = System.lineSeparator()
  def spaces(n: Int): String = {
    " " * n
  }

  def sanitizeInput(input: Char): Either[ErrorMessage,Char] = {
    if(!alphabet.contains(input)){
      Left("Bad input, pick something from: " + alphabet)
    } else {
      Right(input)
    }
  }

  def main(args: Array[String]): Unit = {
    println(createDiamond('F'))
    println(createDiamond('A'))
    println(createDiamond('T'))
    println(createDiamond('~'))
  }

  def createDiamond(rawInput: Char): String = sanitizeInput(rawInput) match {
      case Left(err) => err
      case Right(input) =>
        val indexOfInputChar = alphabet indexOf input.toString.toUpperCase
        val upper = diverge("", indexOfInputChar, 0)
        val lower = converge("", indexOfInputChar, indexOfInputChar)
        upper + lower
  }

  def diverge(accumulator: String, max: Int, counter: Int): String = counter match {
      case `max` => accumulator
      case 0 =>
        val line = spaces(max-counter) + alphabet.charAt(counter) + newline
        diverge(accumulator + line, max, counter + 1)
      case _ =>
        val line = spaces(max-counter) + alphabet.charAt(counter) + spaces(counter * 2 - 1) + alphabet.charAt(counter) + newline
        diverge(accumulator + line, max, counter + 1)
  }

  def converge(accumulator: String, max: Int, counter: Int): String = counter match {
    case 0 => accumulator + spaces(max) + alphabet.charAt(counter) + newline
    case _ =>
      val line = spaces(max-counter) + alphabet.charAt(counter) + spaces(counter * 2 - 1) + alphabet.charAt(counter) + newline
      converge(accumulator + line, max, counter - 1)
  }
}
