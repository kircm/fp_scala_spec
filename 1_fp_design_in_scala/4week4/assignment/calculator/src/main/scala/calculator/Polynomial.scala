package calculator

/**
  * Quadratic Equation
  *
  * http://www.rapidtables.com/math/algebra/Quadratic_equation.htm
  */
object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Signal {
      val currA: Double = a()
      val currB: Double = b()
      val currC: Double = c()

      if (currA == 0.0) Double.NaN
      else (currB * currB) - 4.0 * currA * currC
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      val currA: Double = a()
      val currB: Double = b()
      val currDelta: Double = delta()

      if (currA == 0.0) Set[Double](Double.NaN)
      else if (currDelta < 0) Set[Double]()
      else if (currDelta == 0.0) Set[Double](-currB / 2.0 * currA)
      else {
        val currDeltaRoot = scala.math.sqrt(currDelta)
        val twoA = 2.0 * currA
        val x1 = (-currB + currDeltaRoot) / twoA
        val x2 = (-currB - currDeltaRoot) / twoA

        Set[Double](x1, x2)
      }
    }
  }
}
