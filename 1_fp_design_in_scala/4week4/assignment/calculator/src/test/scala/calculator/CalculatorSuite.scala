package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import calculator.Calculator

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("evaluate simple expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map[String, Signal[Expr]](
      "a" -> Signal(Literal(1.0)),
      "b" -> Signal(Plus(Literal(1.0), Literal(2.0))))

    val namedValues = Calculator.computeValues(namedExpressions)

    assert(namedValues.keySet == namedExpressions.keySet)

    val a: Signal[Double] = namedValues("a")
    assert(a() == 1.0D)

    val b: Signal[Double] = namedValues("b")
    assert(b() == 3.0D)
  }

  test("detect self reference") {
    val namedExpressions: Map[String, Signal[Expr]] = Map[String, Signal[Expr]](
      "a" -> Signal(Literal(1.0)),
      "b" -> Signal(Plus(Literal(1.0), Ref("b"))))

    val namedValues = Calculator.computeValues(namedExpressions)
    val a: Signal[Double] = namedValues("a")
    assert(a() == 1.0D)

    val b: Signal[Double] = namedValues("b")
    assert(b() isNaN)
  }

  test("detect cycle references") {
    val namedExpressions: Map[String, Signal[Expr]] = Map[String, Signal[Expr]](
      "a" -> Signal(Ref("a")),
      "b" -> Signal(Plus(Literal(1.0), Ref("b"))))

    val namedValues = Calculator.computeValues(namedExpressions)
    val a: Signal[Double] = namedValues("a")
    assert(a() isNaN)

    val b: Signal[Double] = namedValues("b")
    assert(b() isNaN)
  }


  test("evaluate complex expression") {
    val namedExpressions: Map[String, Signal[Expr]] = Map[String, Signal[Expr]](
      "a" -> Signal(Literal(1.0)),
      "b" -> Signal(Plus(Literal(1.0), Ref("a"))),
      "c" -> Signal(Plus(Ref("a"), Ref("b"))))

    val namedValues = Calculator.computeValues(namedExpressions)

    val a: Signal[Double] = namedValues("a")
    assert(a() == 1.0D)

    val b: Signal[Double] = namedValues("b")
    assert(b() == 2.0D)

    val c: Signal[Double] = namedValues("c")
    assert(c() == 3.0D)
  }

}
