package recfun
import scala.collection.mutable.ListBuffer

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(level: Int, chars: List[Char]): Int = {
      if (chars.isEmpty || level < 0) level
      else if (chars.head == '(') loop(level + 1, chars.tail)
      else if (chars.head == ')') loop(level - 1, chars.tail)
      else loop(level, chars.tail)
    }

    loop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(coin: Int, count: Int): Int = {
      if (money < (coin * count)) 0
      else countChange(money - coin * count, coins.tail) + loop(coin, count + 1)
    }

    if (coins.isEmpty && money > 0) 0
    else if (money < 0) 0
    else if (money == 0) 1
    else loop(coins.head, 1) + countChange(money, coins.tail)
  }


}
