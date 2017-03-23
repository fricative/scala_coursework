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
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerProcess(chars: List[Char], unclosed: Int): Boolean = {
      if (chars.isEmpty && unclosed != 0) false
      else if (chars.isEmpty && unclosed == 0) true
      else {
        if (chars.head.equals(')')) {
          if (unclosed == 0) false
          else
            innerProcess(chars.tail, unclosed - 1)
        } else if (chars.head.equals('(')) innerProcess(chars.tail, unclosed + 1)
        else innerProcess(chars.tail, unclosed)
      }
    }
    innerProcess(chars, 0)
    //    var open = 0
    //    def innerProcess(chars: List[Char]): Unit = {
    //      if (chars.isEmpty) open = 0
    //      else if (chars.head.equals('(')) {
    //        open = open + 1
    //        innerProcess(chars.tail)
    //      } else if (chars.head.equals(')')) {
    //        open = open - 1
    //        if (open < 0) open = -10000
    //        else
    //          innerProcess(chars.tail)
    //      } else innerProcess(chars.tail)
    //    }
    //    innerProcess(chars)
    //    open == 0
  }

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    def whileConverter(iter: Int, money: Int, coins: List[Int]): Int = {
      if (iter * coins.head > money) 0
      else whileConverter(iter + 1, money, coins) + countChange(money - coins.head * iter, coins.tail)
    }

    if (money == 0) 1 //no money needs to be split
    else if (coins.isEmpty) 0 //there is money, but no more coins
    else { //still has money, still got coins
      whileConverter(0, money, coins)
      //      var i = 0
      //      var ways = 0
      //      while (i * coins.head <= money) {
      //        ways = ways + countChange(money - coins.head * i, coins.tail)
      //        i = i + 1
      //      }
      //      ways
    }
  }
}
