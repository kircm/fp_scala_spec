package recfun

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def validate(chars: List[Char], pars: Int): Boolean = {
      if (pars < 0) return false
      if (chars.isEmpty) pars == 0
      else if (chars.head.equals('(')) validate(chars.tail, pars + 1)
      else if (chars.head.equals(')')) validate(chars.tail, pars - 1)
      else validate(chars.tail, pars)
    }

    validate(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def increaseIfDivisibleBy(money: Int, coin: Int, acc: Int): Int = {
      if ((money % coin) == 0.0) acc + 1
      else acc
    }

    def increaseIfCombinationOfTwo(money: Int, coin1: Int, coin2: Int, acc: Int): Int = {
      val d:Double = Int.int2double(money) / Int.int2double(coin1)
      val m:Double = d % Int.int2double(coin2)
      if (m == 0.0) {
        acc + 1
      }
      else acc
    }

    def combination(money: Int, coins: List[Int], acc: Int): Int = {
      increaseIfDivisibleBy(money, coins.head, acc) +
        increaseIfDivisibleBy(money, coins.tail.head, acc) +
        increaseIfCombinationOfTwo(money, coins.tail.head, coins.head, acc)
    }

    def keepCountingChange(money: Int, coins: List[Int], acc: Int): Int = {
      if (coins.tail.isEmpty) {
        increaseIfDivisibleBy(money, coins.head, acc)
      } else {
        if (coins.tail.tail.isEmpty) {
          combination(money, coins, acc)
        } else {
          combination(money, coins, acc) + keepCountingChange(money, coins.tail, acc)
        }
      }
    }


    if (money <= 0) return 0;
    if (coins.isEmpty) return 0;
    keepCountingChange(money, coins, 0)
  }
}
