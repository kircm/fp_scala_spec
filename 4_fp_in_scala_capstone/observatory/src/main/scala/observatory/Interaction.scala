package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import math._
import scala.collection.parallel.mutable.ParArray

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  // Geo-constants
  val TOTAL_LONGITUDE_WIDTH = 360.0
  val TOTAL_LATITUDE_HEIGHT = 170.0

  // Image size
  // Size required for Grading on week 3
  val IMAGE_WIDTH = 256
  val IMAGE_HEIGHT = 256
  // test size
  //val IMAGE_WIDTH = 360
  //val IMAGE_HEIGHT = 180

  // Transparency factor of image
  val ALPHA: Int = 127

  /*  Range of temperatures and colors - for real  */
  val TEMPERATURE_COLORS: Seq[(Temperature, Color)] = Seq(
    (-60, Color(0, 0, 0)),
    (-50, Color(33, 0, 107)),
    (-27, Color(255, 0, 255)),
    (-15, Color(0, 0, 255)),
    (0, Color(0, 255, 255)),
    (12, Color(255, 255, 0)),
    (32, Color(255, 0, 0)),
    (60, Color(255, 255, 255)))

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val x = tile.x
    val y = tile.y
    val z = tile.zoom

    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << z))))),
      x.toDouble / (1 << z) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val start: Long = System.currentTimeMillis
    val image: Image = calculateImage(tile, temperatures, colors, IMAGE_WIDTH, IMAGE_HEIGHT)
    val end: Long = System.currentTimeMillis

    val timeDiff = end - start
    println(s" timeDiff: $timeDiff")

    image
  }



  // type Data is also defined by the coursera grader
  // type Data = Iterable[(Location, Temperature)]
  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {

    val zoomLevels = 0 to 3

    // for comprehension
    for
      (yd <- yearlyData;
       z <- zoomLevels;
       t <- generateTilesForZoomLevel(z))
      generateImage(yd._1, t, yd._2)

    /* Equivalent: chain of map function calls
    yearlyData
      .map(yd => zoomLevels
        .map(z => generateTilesForZoomLevel(z)
          .map(t => generateImage(yd._1, t, yd._2))))
    */
  }



  /* ------------------------------
     ----- Added  Functions -------
     ------------------------------ */

  def calculateImage(tile: Tile,
                     temperatures: Iterable[(Location, Temperature)],
                     colors: Iterable[(Temperature, Color)],
                     imageWidth: Int,
                     imageHeight: Int):
  Image = {

    val knownTemperaturesInPixelCoords: Iterable[((Int, Int), Temperature)] =
      temperatures.map(p => (latLongToPixelCoord(tile, p._1, imageWidth, imageHeight), p._2))

    val knownTemperaturesByCoord: Map[(Int, Int), Temperature] = knownTemperaturesInPixelCoords.toMap

    def temperatureForIndex(i: Int): Pixel = {
      val coord: (Int, Int) = Visualization.arrayIndexToPixelCoord(i, imageWidth)
      if (knownTemperaturesByCoord.contains(coord))
        Visualization.temperatureToPixelColor(knownTemperaturesByCoord(coord), colors)
      else
        predictedTemperaturePixelColor(tile, coord._1, coord._2, imageWidth, imageHeight, temperatures, colors)
    }

    val pixelArrayPar: ParArray[Pixel] = new ParArray[Pixel](imageWidth * imageHeight)
    val pixelArrayOutput: Array[Pixel] =
      pixelArrayPar.zipWithIndex.map(p => (temperatureForIndex(p._2))).arrayseq.toArray

    Image(imageWidth, imageHeight, pixelArrayOutput)
  }


  def predictedTemperaturePixelColor(tile: Tile,
                                     x: Int,
                                     y: Int,
                                     imageWidth: Int,
                                     imageHeight: Int,
                                     temperatures: Iterable[(Location, Temperature)],
                                     colors: Iterable[(Temperature, Color)])
  : Pixel = {

    val loc: Location = pixelCoordToLatLong(tile, x, y, imageWidth, imageHeight)
    val predictedTemp = Visualization.predictTemperature(temperatures, loc)

    Visualization.temperatureToPixelColor(predictedTemp, colors)
  }


  def latLongToPixelCoord(tile: Tile, location: Location, imageWidth: Int, imageHeight: Int): (Int, Int) = {
    val numTotalTilesInZ0 = math.pow(2.0, tile.zoom)
    val distanceTileLat = TOTAL_LATITUDE_HEIGHT / numTotalTilesInZ0
    val distanceTileLong = TOTAL_LONGITUDE_WIDTH / numTotalTilesInZ0

    val ratioLatitudePixel = distanceTileLat / imageHeight
    val ratioLongitudePixel = distanceTileLong / imageWidth

    // takes into account edge case: longitude +180.0 (would fall off the array)
    val approxX: Double = math.min((location.lon + 180.0) / ratioLongitudePixel, imageWidth - 1)

    // takes into account edge case: latitude +90.0 (would fall off the array)
    // bear in mind North->South latitudes decrease from +90 to -90  while Y's increase as they move Southwards
    val approxY: Double = math.min((math.abs(location.lat + (-90.0)) / ratioLatitudePixel), imageHeight - 1)

    (math.floor(approxX).toInt, math.floor(approxY).toInt)
  }


  def pixelCoordToLatLong(tile: Tile, pixelX: Int, pixelY: Int, imageWidth: Int, imageHeight: Int): Location = {
    val numTotalTilesInZ0 = math.pow(2.0, tile.zoom)
    val distanceTileLat = TOTAL_LATITUDE_HEIGHT / numTotalTilesInZ0
    val distanceTileLong = TOTAL_LONGITUDE_WIDTH / numTotalTilesInZ0

    val topLeftTileLocation = tileLocation(tile)

    val distanceFullPixelLat = distanceTileLat / imageHeight
    val distanceFullPixelLong = distanceTileLong / imageWidth

    val offsetWithinTileLat = distanceFullPixelLat * pixelY
    val offsetWithinTileLon = distanceFullPixelLong * pixelX

    val pixelGeoLocationLat = topLeftTileLocation.lat - offsetWithinTileLat
    val pixelGeoLocationLong = topLeftTileLocation.lon + offsetWithinTileLon

    Location(pixelGeoLocationLat, pixelGeoLocationLong)
  }



  def generateTilesForZoomLevel(zoomLevel: Int): Iterable[Tile] = {
    if (zoomLevel == 0) Iterable(Tile(0, 0, 0))
    else {
      val numRows: Int = math.pow(2, zoomLevel).toInt
      val numTiles: Int = numRows * numRows

      for (indexTile <- 0 to numTiles - 1)
        yield {
          Tile(indexTile / numRows, indexTile % numRows, zoomLevel)
        }
    }
  }

}
