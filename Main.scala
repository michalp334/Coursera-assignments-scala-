package recfun
import scala.annotation.tailrec

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
   * Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r, 
   * counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1, pascal(1,2)=2 
   * and pascal(1,3)=3.
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string, 
   * which we represent as a List[Char] not a String.
   */

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(chars: List[Char], unclosedBrackets: Int): Boolean = {
      if (chars.isEmpty) unclosedBrackets == 0
      else {
        val char = chars.head
        char match {
          case '(' => checkBalance(chars.tail, unclosedBrackets + 1)
          case ')' =>
            if (unclosedBrackets > 0) checkBalance(chars.tail, unclosedBrackets - 1)
            else false
          case _ => checkBalance(chars.tail, unclosedBrackets)
        }
      }
    }
    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make change for an amount, 
   * given a list of coin denominations. 
   * For example, there are 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
