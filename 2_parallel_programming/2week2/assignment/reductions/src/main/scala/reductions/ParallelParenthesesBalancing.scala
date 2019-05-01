package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    if (chars.length == 0) true
    else balanceRec(chars, 0, 0) == 0
  }

  def balanceRec(chars: Array[Char], currentChar: Int, openPars: Int): Int = {
    if (openPars < 0) return openPars

    if (currentChar == chars.length && openPars == 0) openPars
    else if (currentChar == chars.length) openPars
    else {
      if (chars(currentChar) == '(') balanceRec(chars, currentChar + 1, openPars + 1)
      else if (chars(currentChar) == ')') balanceRec(chars, currentChar + 1, openPars - 1)
      else balanceRec(chars, currentChar + 1, openPars)
    }
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int, Int, Int) = {
      var openPars = if (chars(idx) == '(') 1 else 0
      var closedPars = if (chars(idx) == ')') 1 else 0
      var negativePars = closedPars
      var positivePars = openPars

      var i = idx + 1
      while (i < until) {
        if (chars(i) == '(') {
          openPars += 1
          positivePars += 1
        }
        if (chars(i) == ')') {
          closedPars += 1
          positivePars -= 1
          positivePars = Math.max(positivePars, 0)
          if ((closedPars - openPars) > negativePars) negativePars += 1
        }
        i += 1
      }

      val s: String = new String(chars, idx, until - idx)
      println(s"traverse $s   " + (openPars, closedPars, negativePars, positivePars))

      (openPars, closedPars, negativePars, positivePars)
    }

    def reduceSeg(from: Int, until: Int, arg1: Int, arg2: Int): (Int, Int, Int, Int) = {
      if (threshold >= until - from) traverse(from, until, arg1, arg2)
      else {
        val mid = from + (until - from) / 2

        val (res1, res2) = parallel(
          reduceSeg(from, mid, 0, 0),
          reduceSeg(mid, until, from, mid))

        calculateBalancesFromPartialResults(res1, res2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val (open, closed, negative, positive) = reduceSeg(from, until, 0, 0)
      println((open, closed, negative, positive))
      (open - closed, negative)
    }

    reduce(0, chars.length) == (0, 0)
  }

  private def calculateBalancesFromPartialResults(resultLeft: (Int, Int, Int, Int), resultRight: (Int, Int, Int, Int)): (Int, Int, Int, Int) = {
    val openParsLeft = resultLeft._1
    val closedParsLeft = resultLeft._2
    val openParsRight = resultRight._1
    val closedParsRight = resultRight._2
    val negativeParsLeft = resultLeft._3
    val negativeParsRight = resultRight._3
    val positiveParsLeft = resultLeft._4
    val positiveParsRight = resultRight._4

    val openParsTotal = openParsLeft + openParsRight
    val closedParsTotal = closedParsLeft + closedParsRight

    val negativeParsRightCompensated = Math.max(negativeParsRight - positiveParsLeft, 0)
    val positiveParsLeftUsed = Math.min(positiveParsLeft, negativeParsRight)

    val negativeParsTotal = negativeParsLeft + negativeParsRightCompensated
    val positiveParsTotal = (positiveParsLeft - positiveParsLeftUsed) + positiveParsRight

    println("(openParsTotal, closedParsTotal, negativeParsTotal, positiveParsTotal)  " + (openParsTotal, closedParsTotal, negativeParsTotal, positiveParsTotal))
    (openParsTotal, closedParsTotal, negativeParsTotal, positiveParsTotal)
  }
}
