package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import observatory.TestUtils._

trait ManipulationTest extends FunSuite with Checkers {

  test("make grid") {
    val temperatures: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 3.0),
      (Location(0.0, 0.0), 4.0),
      (Location(60, -90.0), 5.0),
      (Location(80, -179), 6.0))

    val output = Manipulation.makeGrid(temperatures)

    val gridLocation1 = GridLocation(-30, 80)
    val gridLocation2 = GridLocation(0, 0)
    val gridLocation3 = GridLocation(30, -89)
    assertWithMarginOfError(output(gridLocation1), 3.0, 0.01)
    assertWithMarginOfError(output(gridLocation2), 4.0, 0.01)
    assertWithMarginOfError(output(gridLocation3), 4.8, 0.01)
  }

  test("average") {
    val temperaturesYear1: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 3.0),
      (Location(0.0, 0.0), 4.0),
      (Location(60.0, -90.0), 5.0),
      (Location(80.0, -179.0), 6.0))

    val temperaturesYear2: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 5.0),
      (Location(0.0, 0.0), 8.0),
      (Location(60.0, -90.0), 15.0),
      (Location(80.0, -179.0), 6.0))

    val temperaturesYears = Iterable(temperaturesYear1, temperaturesYear2)

    val output = Manipulation.average(temperaturesYears)

    val gridLocation1 = GridLocation(-30, 80)
    val gridLocation2 = GridLocation(0, 0)
    val gridLocation3 = GridLocation(30, -89)

    assertWithMarginOfError(output(gridLocation1), 4.0, 0.01)
    assertWithMarginOfError(output(gridLocation2), 6.0, 0.01)
    assertWithMarginOfError(output(gridLocation3), 8.3, 0.01)
  }

  test("deviation") {
    val temperaturesKnown: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 4.0),
      (Location(0.0, 0.0), 4.0),
      (Location(60.0, -90.0), 5.0),
      (Location(80.0, -179.0), 8.0))

    val temperaturesYear1: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 3.0),
      (Location(0.0, 0.0), 4.0),
      (Location(60.0, -90.0), 5.0),
      (Location(80.0, -179.0), 6.0))

    val temperaturesYear2: Iterable[(Location, Temperature)] = Iterable(
      (Location(-89.0, 179.0), 1.0),
      (Location(-60.0, 120.0), 2.0),
      (Location(-30.0, 80.0), 5.0),
      (Location(0.0, 0.0), 8.0),
      (Location(60.0, -90.0), 15.0),
      (Location(80.0, -179.0), 6.0))

    val temperaturesNormal = Manipulation.average(Iterable(temperaturesYear1, temperaturesYear2))

    val output = Manipulation.deviation(temperaturesKnown, temperaturesNormal)

    val gridLocation1 = GridLocation(-30, 80)
    val gridLocation2 = GridLocation(0, 0)
    val gridLocation3 = GridLocation(80, -179)

    assertWithMarginOfError(output(gridLocation1), 0.0, 0.01)
    assertWithMarginOfError(output(gridLocation2), -2.0, 0.01)
    assertWithMarginOfError(output(gridLocation3), 2.0, 0.01)
  }


}