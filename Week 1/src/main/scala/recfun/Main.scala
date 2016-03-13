package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else
      pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
  {
    def isOpeningBracketCharacter(character: Char) =
      character == '('

    def isClosingBracketCharacter(character: Char) =
      character == ')'

    def verifyEmptyList(counter: Int) =
      counter == 0

    def isBalanced(counter: Int, chars: List[Char]): Boolean =

      if (chars.isEmpty)
        verifyEmptyList(counter)
      else if (counter < 0)
        false
      else if (isOpeningBracketCharacter(chars.head))
        isBalanced(counter+1, chars.tail)
      else if (isClosingBracketCharacter(chars.head))
        isBalanced(counter-1, chars.tail)
      else
        isBalanced(counter, chars.tail)

    isBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
