package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinear interpolation zero") {
    val point = CellPoint(0.0, 0.0)
    val d00 = 0.0
    val d01 = 0.0
    val d10 = 0.0
    val d11 = 0.0

    val output = Visualization2.bilinearInterpolation(point, d00, d01, d10, d11)

    assert(output == 0.0)
  }

  test("bilinear interpolation middle") {
    val point = CellPoint(0.5, 0.5)
    val d00 = 0.0
    val d01 = 0.4
    val d10 = 0.0
    val d11 = 0.4

    val output = Visualization2.bilinearInterpolation(point, d00, d01, d10, d11)

    println(output)
    assert(output == 0.2)
  }

}
