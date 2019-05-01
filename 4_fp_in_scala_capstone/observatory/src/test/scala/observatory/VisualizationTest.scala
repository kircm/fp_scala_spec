package observatory


import java.awt.image.BufferedImage
import java.io.File

import com.sksamuel.scrimage.Image
import javax.imageio.ImageIO
import observatory.Visualization._
import observatory.TestVals._
import observatory.TestUtils._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  val ERROR_THRESHOLD_IN_KM = 10.0

  test("distance zero") {
    val location1 = Location(123.45, 678.90)
    val location2 = Location(123.45, 678.90)

    val output = d(location1, location2)

    assert(output == 0.0)
  }

  test("distance antipodes") {
    val location1 = Location(27.97, -82.53)
    val location2 = Location(-27.97, 97.47)

    val output = d(location1, location2)

    assert(output.equals(MEDIAN_EARTH_RADIUS_KM * math.Pi))
  }

  test("distance St Petersburg to SF") {
    // Test distances
    // https://keisan.casio.com/exec/system/1224587128
    val location1 = Location(59.9, -30.3)
    val location2 = Location(37.8, 122.4)

    val output = d(location1, location2)

    assert(math.abs(8861 - output) < ERROR_THRESHOLD_IN_KM)
  }

  test("distance Paris to Austin") {
    // Test distances
    // https://keisan.casio.com/exec/system/1224587128
    val location1 = Location(48.87, -2.33)
    val location2 = Location(30.27, 97.74)

    val output = d(location1, location2)

    assert(math.abs(8195 - output) < ERROR_THRESHOLD_IN_KM)
  }

  test("weight of two locations") {
    val location1 = Location(0.0, 0.0)
    val location2 = Location(0.018, 0.0)

    val output = w(location1, location2)

    assert(math.abs(0.25 - output) < 0.01)
  }

  test("predict temperature when same location") {
    val knownTemperatures: Iterable[(Location, Temperature)] =
      Iterable((Location(0.0, 0.0), 0.0),
        (Location(100.0, 0.0), 15.0),
        (Location(200.0, 0.0), 30.0))

    val output = predictTemperature(knownTemperatures, Location(100.0, 0.0))

    assert(output.equals(15.0))
  }

  test("predict temperature when close location") {
    val knownTemperatures: Iterable[(Location, Temperature)] =
      Iterable((Location(0.0, 0.0), 0.0),
        (Location(100.0, 0.0), 15.0),
        (Location(200.0, 0.0), 30.0))

    val output = predictTemperature(knownTemperatures, Location(200.001, 0.0))

    assert(output.equals(30.0))
  }

  test("sum of weights when only 1 known location") {
    val temperatures = Iterable[(Location, Temperature)]((Location(0.0, 0.0), 0.0))

    val output = sumW(temperatures, Location(0.018, 0.0))

    assert(math.abs(0.25 - output) < 0.01)
  }

  test("sum of weights when 2 known locations") {
    val temperatures = Iterable[(Location, Temperature)](
      (Location(0.0, 0.0), 0.0),
      (Location(0.0, 0.0), 0.0))

    val output = sumW(temperatures, Location(0.018, 0.0))

    assert(math.abs(0.50 - output) < 0.01)
  }

  test("predict temperature known locations all same temp") {
    val temperatures: Iterable[(Location, Temperature)] =
      Iterable[(Location, Temperature)]((Location(1.0, 1.0), 0.0), (Location(1.0, 2.0), 0.0))

    val output = predictTemperature(temperatures, Location(1.0, 3.0))

    assert(output.equals(0.0))
  }

  test("predict temperature known locations different temperatures close location") {
    val temperatures: Iterable[(Location, Temperature)] =
      Iterable[(Location, Temperature)]((Location(1.0, 1.0), 10.0), (Location(1.0, 2.0), 0.0))

    val output = predictTemperature(temperatures, Location(1.0, 2.001))

    assert(output.equals(0.0))
  }

  test("predict temperature known locations different temperatures average") {
    val temperatures: Iterable[(Location, Temperature)] =
      Iterable[(Location, Temperature)]((Location(1.0, 1.0), 10.0), (Location(1.0, 2.0), 0.0))

    val output = predictTemperature(temperatures, Location(1.0, 1.5))

    assert(output.equals(5.0))
  }

  test("interpolate color component simple") {
    val point1: (Temperature, Int) = (0, 0)
    val point2: (Temperature, Int) = (4, 4)
    val tempX: Temperature = 2

    val output = interpolateColorComponent(point1, point2, tempX)

    assert(output.equals(2))
  }

  test("interpolate color component") {
    val point1: (Temperature, Int) = (1, 4)
    val point2: (Temperature, Int) = (4, 16)
    val tempX: Temperature = 2

    val output = interpolateColorComponent(point1, point2, tempX)

    assert(output.equals(8))
  }

  test("interpolate color component same temperature") {
    val point1: (Temperature, Int) = (1, 4)
    val point2: (Temperature, Int) = (4, 16)
    val tempX: Temperature = 4

    val output = interpolateColorComponent(point1, point2, tempX)

    assert(output.equals(16))
  }

  test("interpolate color temperature below lowest") {
    val lowTemp: Temperature = -80.0

    val output = interpolateColor(TEMPERATURE_COLORS_TEST, lowTemp)

    assert(output.equals(Color(0, 0, 0)))
  }

  test("interpolate color temperature above highest") {
    val lowTemp: Temperature = 80.0

    val output = interpolateColor(TEMPERATURE_COLORS_TEST, lowTemp)

    assert(output.equals(Color(255, 255, 255)))
  }

  test("interpolate color temperature is 12") {
    val temperature: Temperature = 12.0

    val output = interpolateColor(TEMPERATURE_COLORS_TEST, temperature)

    assert(output.equals(Color(255, 255, 0)))
  }

  test("interpolate color temperature is 33") {
    val temperature: Temperature = 33.0

    val output = interpolateColor(TEMPERATURE_COLORS_TEST, temperature)

    assert(output.equals(Color(255, 9, 9)))  // Very close to Red
  }

  test("interpolate color temperature is 59") {
    val temperature: Temperature = 59.0

    val output = interpolateColor(TEMPERATURE_COLORS_TEST, temperature)

    assert(output.equals(Color(255, 246, 246))) // Very close to White
  }

  test("interpolate color temperature is using coursera grading scale for test") {
    val temperature: Temperature = -60.0
    val scale: Seq[(Temperature, Color)] = Seq((-80.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255)))

    val output = interpolateColor(scale, temperature)

    // Color(191,0,64)
    assert(output.equals(Color(191, 0, 64)))
  }

  test("interpolate color temperature is using coursera grading scale for test 2") {
    val temperature: Temperature = -84.89818822933492
    val scale: Seq[(Temperature, Color)] = Seq((-84.89818822933492, Color(255,0,0)), (100.0, Color(0,0,255)))

    val output = interpolateColor(scale, temperature)

    // Color(255,0,0)
    assert(output.equals(Color(255, 0, 0)))
  }

  test("convert array index to pixel coord 0") {
    val i: Int = 0

    val output = arrayIndexToPixelCoord(i, IMAGE_WIDTH)

    assert(output.equals((0, 0)))
  }

  test("convert array index to pixel coord 1") {
    val i: Int = 1

    val output = arrayIndexToPixelCoord(i, IMAGE_WIDTH)

    assert(output.equals((1, 0)))
  }

  test("convert array index to pixel coord last pixel on first row") {
    val i: Int = IMAGE_WIDTH - 1

    val output = arrayIndexToPixelCoord(i, IMAGE_WIDTH)

    assert(output.equals((IMAGE_WIDTH - 1, 0)))
  }

  test("convert array index to pixel coord first pixel on second row") {
    val i: Int = IMAGE_WIDTH

    val output = arrayIndexToPixelCoord(i, IMAGE_WIDTH)

    assert(output.equals((0, 1)))
  }

  test("convert array index to pixel coord last pixel on image") {
    val i: Int = (IMAGE_WIDTH * IMAGE_HEIGHT) - 1

    val output = arrayIndexToPixelCoord(i, IMAGE_WIDTH)

    assert(output.equals((IMAGE_WIDTH - 1, IMAGE_HEIGHT - 1)))
  }

  test("convert lat/long to pixel coord top left") {
    val location = Location(90, -180)

    val output = latLongToPixelCoord(location, IMAGE_WIDTH, IMAGE_HEIGHT)

    assert(output.equals((0, 0)))
  }

  test("convert lat/long to pixel coord top right") {
    val location = Location(90, 180)

    val output = latLongToPixelCoord(location, IMAGE_WIDTH, IMAGE_HEIGHT)

    assert(output.equals((IMAGE_WIDTH - 1, 0)))
  }

  test("convert lat/long to pixel coord bottom left") {
    val location = Location(-90, -180)

    val output = latLongToPixelCoord(location, IMAGE_WIDTH, IMAGE_HEIGHT)

    assert(output.equals((0, IMAGE_HEIGHT - 1)))
  }

  test("convert lat/long to pixel coord bottom right") {
    val location = Location(-90, 180)

    val output = latLongToPixelCoord(location, IMAGE_WIDTH, IMAGE_HEIGHT)

    assert(output.equals((IMAGE_WIDTH - 1, IMAGE_HEIGHT - 1)))
  }

  test("convert lat/long to pixel coord center") {
    val location = Location(0, 0)

    val output = latLongToPixelCoord(location, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output._1, IMAGE_WIDTH / 2)
    assertWithMarginOfError(output._2, IMAGE_HEIGHT / 2)
  }

  test("convert pixel coord to lat long upper left") {
    val x: Int = 0
    val y: Int = 0

    val output = pixelCoordToLatLong(x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("convert pixel coord to lat long upper right") {
    val x: Int = IMAGE_WIDTH - 1
    val y: Int = 0

    val output = pixelCoordToLatLong(x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, 180.0)
  }

  test("convert pixel coord to lat long lower left") {
    val x: Int = 0
    val y: Int = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, -85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("convert pixel coord to lat long lower right") {
    val x: Int = IMAGE_WIDTH - 1
    val y: Int = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, -85.0)
    assertWithMarginOfError(output.lon, 178.0)
  }

  test("convert pixel coord to lat long center") {
    val x: Int = IMAGE_WIDTH / 2
    val y: Int = IMAGE_HEIGHT / 2

    val output = pixelCoordToLatLong(x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 0.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("convert pixel coord to array index (0, 0)") {
    val x: Int = 0
    val y: Int = 0

    val output = pixelCoordToArrayIndex(x, y, IMAGE_WIDTH)

    assert(output.equals(0))
  }

  test("convert pixel coord to array index last pixel on first row") {
    val x: Int = IMAGE_WIDTH - 1
    val y: Int = 0

    val output = pixelCoordToArrayIndex(x, y, IMAGE_WIDTH)

    assert(output.equals(IMAGE_WIDTH - 1))
  }

  test("convert pixel coord to array index (0, 1)") {
    val x: Int = 0
    val y: Int = 1

    val output = pixelCoordToArrayIndex(x, y, IMAGE_WIDTH)

    assert(output.equals(IMAGE_WIDTH))
  }

  test("convert pixel coord to array index last pixel on image") {
    val x: Int = IMAGE_WIDTH - 1
    val y: Int = IMAGE_HEIGHT - 1

    val output = pixelCoordToArrayIndex(x, y, IMAGE_WIDTH)

    assert(output.equals((IMAGE_WIDTH * IMAGE_HEIGHT) - 1))
  }

  test("visualize with two known temperatures") {
    val arg0 = -100.0
    val arg1 = 43.847158906958526

    val location1 = Location(45.0, -90.0)
    val location2 = Location(-45.0, 0.0)

    val knownTemperatures: Iterable[(Location, Temperature)] = Iterable(
      (location1, arg0),
      (location2, arg1))

    val scale: Iterable[(Temperature, Color)] = Iterable(
      (arg0, Color(255, 0, 0)),
      (arg1, Color(0, 0, 255)))

    val output = visualize(knownTemperatures, scale)

    val coordLocation1 = latLongToPixelCoord(location1, IMAGE_WIDTH, IMAGE_HEIGHT)
    val coordLocation2 = latLongToPixelCoord(location2, IMAGE_WIDTH, IMAGE_HEIGHT)
    println(s"coordLocation1: $coordLocation1  coordLocation2: $coordLocation2  ")
    // coordLocation1: (90, 45)
    // coordLocation2: (180, 135)

    val r1 = output.pixel(coordLocation1._1, coordLocation1._2).red
    val g1 = output.pixel(coordLocation1._1, coordLocation1._2).green
    val b1 = output.pixel(coordLocation1._1, coordLocation1._2).blue
    println(s"RGB location 1  (90, 45) $r1 $g1 $b1")

    val r2 = output.pixel(coordLocation2._1, coordLocation2._2).red
    val g2 = output.pixel(coordLocation2._1, coordLocation2._2).green
    val b2 = output.pixel(coordLocation2._1, coordLocation2._2).blue
    println(s"RGB location 2  (180, 135) $r2 $g2 $b2")

    val coordLocationX1 = latLongToPixelCoord(location1, IMAGE_WIDTH, IMAGE_HEIGHT)
    val rx1 = output.pixel(coordLocationX1._1, coordLocationX1._2).red
    val gx1 = output.pixel(coordLocationX1._1, coordLocationX1._2).green
    val bx1 = output.pixel(coordLocationX1._1, coordLocationX1._2).blue
    println(s"RGB location X1 (100, 60) $rx1 $gx1 $bx1")

    val coordLocationX2 = latLongToPixelCoord(location2, IMAGE_WIDTH, IMAGE_HEIGHT)
    val rx2 = output.pixel(coordLocationX2._1, coordLocationX2._2).red
    val gx2 = output.pixel(coordLocationX2._1, coordLocationX2._2).green
    val bx2 = output.pixel(coordLocationX2._1, coordLocationX2._2).blue
    println(s"RGB location X2 (195, 175) $rx2 $gx2 $bx2")

    // Assert that RGB colors in location x1 are closer to known location 1 than to known location 2
    assert(math.abs(rx1 - r1) <= math.abs(rx1 - r2))
    assert(math.abs(gx1 - g1) <= math.abs(gx1 - g2))
    assert(math.abs(bx1 - b1) <= math.abs(bx1 - b2))

    // Assert that RGB colors in location x2 are closer to known location 2 than to known location 1
    assert(math.abs(rx2 - r2) <= math.abs(rx2 - r1))
    assert(math.abs(gx2 - g2) <= math.abs(gx2 - g1))
    assert(math.abs(bx2 - b2) <= math.abs(bx2 - b1))
  }

  // --------------------------------------------------
  // ---------- Testing image and file generation------
  // --------------------------------------------------
  test("generate test image in output directory") {
    val output: Image = visualize(knownTemperatures, TEMPERATURE_COLORS_TEST)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    // Generate an output JPEG file
    /*
    val img: BufferedImage  = output.toNewBufferedImage()
    val f: File = new File("./output/OutputImage.jpeg")
    ImageIO.write(img, "JPEG", f)
    */
  }
}
