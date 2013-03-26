package recfun
import common._

object Main {
  def main(args : Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1: Pascal's Triangle
   */
  def pascal(c : Int, r : Int) : Int =
    if (r < c) 0
    else if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2: Parentheses Balancing
   */
  def balance(chars : List[Char]) : Boolean = {

    def balanceIter(chars : List[Char], count : Int) : Boolean =
      if (chars.isEmpty) count == 0
      else if (chars.head.equals(')') && count <= 0) false
      else if (chars.head.equals(')') && count > 0) balanceIter(chars.tail, count - 1)
      else if (chars.head.equals('(')) balanceIter(chars.tail, count + 1)
      else balanceIter(chars.tail, count)

    balanceIter(chars, 0)
  }

  /**
   * Exercise 3: Counting Change
   */
  def countChange(money : Int, coins : List[Int]) : Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
