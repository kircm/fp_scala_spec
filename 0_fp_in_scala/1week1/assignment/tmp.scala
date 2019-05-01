  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def smallestDen(coins: List[Int]): Int = {
      def minB(coins: List[Int], currentMin: Int): Int = {
        if (coins.isEmpty) return currentMin
        else if (coins.head < currentMin && currentMin >= 0) minB(coins.tail, coins.head)
        else minB(coins.tail, currentMin)
      }

      if (!coins.isEmpty) minB(coins, coins.head)
      else return 0   // empty list
    }

    def keepCountingWithSmallestDen(money: Int, coins: List[Int], numberOfWays: Int): Int = {
      if (money < 0) return numberOfWays
      else if (money == 0) return numberOfWays + 1
      else numberOfWays + keepCountingWithSmallestDen(money - smallestDen(coins), coins, numberOfWays)
    }

    def keepCounting(money: Int, coins: List[Int], numberOfWays: Int): Int = {
      if (money <= 0 || coins.isEmpty) return numberOfWays
      else {
        keepCountingWithSmallestDen(money, coins, numberOfWays) +
          keepCountingWithSmallestDen(money, coins.tail, numberOfWays)
      }
    }

    keepCounting(money, coins, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def increaseIfCurrentCoin(money: Int, coins: List[Int]): Int = {
      if ((money % coins.head) == 0) 1 + countChange(money, coins.tail)
      else countChange(money, coins.tail)
    }

    if (money <= 0) return 0;
    if (coins.isEmpty) return 0;

    if (!coins.tail.isEmpty) {
      if (coins.tail.tail.isEmpty) {
        increaseIfCurrentCoin(money, coins) + increaseIfCurrentCoin(money, coins.tail)
      } else {
        countChange(money, coins.tail.tail)
      }
    } else {
      increaseIfCurrentCoin(money, coins)
    }
  }



