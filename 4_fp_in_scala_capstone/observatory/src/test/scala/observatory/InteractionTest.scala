package observatory

import com.sksamuel.scrimage.Image
import observatory.Interaction._
import observatory.TestVals._
import observatory.TestUtils._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  test("tile location (0, 0, 0") {
    val tile: Tile = Tile(0, 0, 0)

    val output = tileLocation(tile)

    println(output)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("tile location (1, 0, 1)") {
    val tile: Tile = Tile(1, 0, 1)

    val output = tileLocation(tile)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile location (1, 1, 1)") {
    val tile: Tile = Tile(1, 1, 1)

    val output = tileLocation(tile)

    assertWithMarginOfError(output.lat, 0.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile pixel geo-location zoom 0 top left corner") {
    val tile = Tile(0, 0, 0)
    val x = 0
    val y = 0

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("tile pixel geo-location zoom 0 top right corner") {
    val tile = Tile(0, 0, 0)
    val x = IMAGE_WIDTH - 1
    val y = 0

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, 180.0)
  }

  test("tile pixel geo-location zoom 0 top center") {
    val tile = Tile(0, 0, 0)
    val x = math.floor(IMAGE_WIDTH / 2.0).toInt
    val y = 0

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile pixel geo-location zoom 0 bottom left") {
    val tile = Tile(0, 0, 0)
    val x = 0
    val y = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, -85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("tile pixel geo-location zoom 0 bottom right") {
    val tile = Tile(0, 0, 0)
    val x = IMAGE_WIDTH - 1
    val y = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, -85.0)
    assertWithMarginOfError(output.lon, 180.0)
  }

  test("tile pixel geo-location zoom 0 bottom center") {
    val tile = Tile(0, 0, 0)
    val x = math.floor(IMAGE_WIDTH / 2.0).toInt
    val y = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, -85.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile pixel geo-location zoom 0 center") {
    val tile = Tile(0, 0, 0)
    val x = math.floor(IMAGE_WIDTH / 2.0).toInt
    val y = math.floor(IMAGE_HEIGHT / 2.0).toInt

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 0.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile pixel geo-location zoom 1 top left tile 0 offset") {
    val tile = Tile(0, 0, 1)
    val x = 0
    val y = 0

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 85.0)
    assertWithMarginOfError(output.lon, -180.0)
  }

  test("tile pixel geo-location zoom 1 top left tile offset bottom right") {
    val tile = Tile(0, 0, 1)
    val x = IMAGE_WIDTH - 1
    val y = IMAGE_HEIGHT - 1

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 0.0)
    assertWithMarginOfError(output.lon, 0.0)
  }

  test("tile pixel geo-location zoom 1 top left tile offset center") {
    val tile = Tile(0, 0, 1)
    val x = math.floor(IMAGE_WIDTH / 2.0).toInt
    val y = math.floor(IMAGE_HEIGHT / 2.0).toInt

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    assertWithMarginOfError(output.lat, 42.5)
    assertWithMarginOfError(output.lon, -90.0)
  }

  test("tile pixel geo-location zoom 1 bottom right tile offset center") {
    val tile = Tile(1, 1, 1)
    val x = math.floor(IMAGE_WIDTH / 2.0).toInt
    val y = math.floor(IMAGE_HEIGHT / 2.0).toInt

    val output = pixelCoordToLatLong(tile, x, y, IMAGE_WIDTH, IMAGE_HEIGHT)

    println(output)


    assertWithMarginOfError(output.lat, -42.5)
    assertWithMarginOfError(output.lon, 90.0)
  }

  // --------------------------------------------------
  // ---------- Testing image and file generation------
  // --------------------------------------------------

  test("generate tile test image in output directory (0, 0, 0)") {
    val tileX: Int = 0
    val tileY: Int = 0
    val tileZoom: Int = 0
    val tile1: Tile = Tile(tileX, tileY, tileZoom)

    val output: Image = tile(knownTemperatures2, TEMPERATURE_COLORS_TEST, tile1)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    TestUtils.dumpImageBytesToFile(tileZoom, tileX, tileY, output.toNewBufferedImage())
  }

  test("generate tile test image in output directory (0, 0, 1)") {
    val tileX: Int = 0
    val tileY: Int = 0
    val tileZoom: Int = 1
    val tile1: Tile = Tile(tileX, tileY, tileZoom)

    val output: Image = tile(knownTemperatures2, TEMPERATURE_COLORS_TEST, tile1)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    TestUtils.dumpImageBytesToFile(tileZoom, tileX, tileY, output.toNewBufferedImage())
  }

  test("generate tile test image in output directory (1, 0, 1)") {
    val tileX: Int = 1
    val tileY: Int = 0
    val tileZoom: Int = 1
    val tile1: Tile = Tile(tileX, tileY, tileZoom)

    val output: Image = tile(knownTemperatures2, TEMPERATURE_COLORS_TEST, tile1)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    TestUtils.dumpImageBytesToFile(tileZoom, tileX, tileY, output.toNewBufferedImage())
  }

  test("generate tile test image in output directory (0, 1, 1)") {
    val tileX: Int = 0
    val tileY: Int = 1
    val tileZoom: Int = 1
    val tile1: Tile = Tile(tileX, tileY, tileZoom)

    val output: Image = tile(knownTemperatures2, TEMPERATURE_COLORS_TEST, tile1)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    TestUtils.dumpImageBytesToFile(tileZoom, tileX, tileY, output.toNewBufferedImage())
  }

  test("generate tile test image in output directory (1, 1, 1)") {
    val tileX: Int = 1
    val tileY: Int = 1
    val tileZoom: Int = 1
    val tile1: Tile = Tile(tileX, tileY, tileZoom)

    val output: Image = tile(knownTemperatures2, TEMPERATURE_COLORS_TEST, tile1)
    assert(output.count == IMAGE_WIDTH * IMAGE_HEIGHT)

    TestUtils.dumpImageBytesToFile(tileZoom, tileX, tileY, output.toNewBufferedImage())
  }

  /*
   * A test for generateTiles - function called by the coursera grader
   * with their own definition for type: Data
   *
  test("generate tiles") {
    val yearlyData: Iterable[(Year, Data)] = Iterable((2015, knownTemperatures2))

    generateTiles(yearlyData, generateImageForYearForTile)
  }
  / */

  /**
    * MK: A function that can be passed as generateImage
    * parameter when calling generateTiles
    */
  def generateImageForYearForTile(year: Year,
                                  tileToGenerate: Tile,
                                  knownTemperatures: Iterable[(Location, Temperature)]): Unit = {

    println(s"generateImageForYearForTile  $year $tileToGenerate")

    val image: Image = tile(knownTemperatures, TEMPERATURE_COLORS, tileToGenerate)

    Utils.dumpImageBytesToFile(year, tileToGenerate.zoom, tileToGenerate.x, tileToGenerate.y, image.toNewBufferedImage())
  }


}
