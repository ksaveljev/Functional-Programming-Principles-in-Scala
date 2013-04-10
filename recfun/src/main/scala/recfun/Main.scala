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
  def pascal(c: Int, r: Int): Int = {
    (r, c) match {
      case (0, _) => 1
      case (_, 0) => 1
      case (r, c) if r == c => 1
      case (r, c) => pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def verify(counter: Int, chars: List[Char]): Boolean = {
      chars match {
        case _ if counter < 0 => false
        case Nil => counter == 0
        case '(' :: xs => verify(counter + 1, xs)
        case ')' :: xs => verify(counter - 1, xs)
        case _ :: xs => verify(counter, xs)
      }
    }

    verify(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (_, Nil) => 0
      case (money, _) if money < 0 => 0
      case (money, x :: xs) => countChange(money - x, x::xs) + countChange(money, xs)
    }
  }
}
