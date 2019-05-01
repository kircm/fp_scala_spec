package observatory

import java.time.{LocalDate, LocalDateTime}

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.parallel.mutable.ParArray
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  // Geo-constants
  val TOTAL_LONGITUDE_WIDTH = 360.0
  val TOTAL_LATITUDE_HEIGHT = 180.0
  val LONGITUDE_OFFSET = 180.0
  val LATITUDE_OFFSET = 90.0

  // Image size
  // Size required for Grading on week 2
  val IMAGE_WIDTH = 360
  val IMAGE_HEIGHT = 180
  // test size
  //val IMAGE_WIDTH = 255
  //val IMAGE_HEIGHT = 255

  // Transparency factor of image
  val ALPHA: Int = 127

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def calculateLongDistanceTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
      val sumWeights: Double = sumW(temperatures, location)
      val sumWeightsTemps: Double = sumWT(temperatures, location)

      sumWeightsTemps / sumWeights
    }

    val shortDistance: Option[(Location, Temperature)] = temperatures.find(p => d(p._1, location) < 1)
    if (shortDistance.isDefined) shortDistance.get._2
    else calculateLongDistanceTemperature(temperatures, location)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def calculateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
      val interval: ((Temperature, Color), (Temperature, Color)) = findInterval(points, value)

      val redComponent = interpolateColorComponent((interval._1._1, interval._1._2.red), (interval._2._1, interval._2._2.red), value)
      val greenComponent = interpolateColorComponent((interval._1._1, interval._1._2.green), (interval._2._1, interval._2._2.green), value)
      val blueComponent = interpolateColorComponent((interval._1._1, interval._1._2.blue), (interval._2._1, interval._2._2.blue), value)

      Color(redComponent, greenComponent, blueComponent)
    }

    val sortedPoints = points.toSeq.sortBy(_._1)

    val minTemp: Temperature = sortedPoints.head._1
    val maxTemp: Temperature = sortedPoints.last._1

    if (value <= minTemp) sortedPoints.head._2
    else if (value >= maxTemp) sortedPoints.last._2
    else calculateColor(points, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val start: Long = System.currentTimeMillis
    val image: Image = calculateImage(temperatures, colors, IMAGE_WIDTH, IMAGE_HEIGHT)
    val end: Long = System.currentTimeMillis

    val timeDiff = end - start
    println(s" timeDiff: $timeDiff")

    image
  }



  /* ------------------------------
     ----- Added Functions -------
     ------------------------------ */


  def d(location1: Location, location2: Location): Double = {
    def multiplyByEarthRadius(d: Double): Double = MEDIAN_EARTH_RADIUS_KM * d

    def calculateCentralAngle(location1: Location, location2: Location): Double = {
      // https://en.wikipedia.org/wiki/Great-circle_distance#Formulas

      val phi1 = math.toRadians(location1.lat)
      val phi2 = math.toRadians(location2.lat)

      val lambda1 = math.toRadians(location1.lon)
      val lambda2 = math.toRadians(location2.lon)

      val deltaLambda = abs(lambda1 - lambda2)

      // central angle
      acos(sin(phi1) * sin(phi2) + cos(phi1) * cos(phi2) * cos(deltaLambda))
    }

    def areAntipodes(location1: Location, location2: Location): Boolean = location1.equals(antipodesForLocation(location2))

    def antipodesForLocation(location: Location): Location = {
      val antipodesLatitude = -location.lat
      val longitude1Diff180 = LONGITUDE_OFFSET - abs(location.lon)
      val antipodesLongitude = if (location.lon < 0.0D) longitude1Diff180 else -longitude1Diff180
      Location(antipodesLatitude, antipodesLongitude)
    }



    def greatCircleDistance(location1: Location, location2: Location): Double = {
      if (location1.equals(location2)) 0.0D
      else if (areAntipodes(location1, location2)) multiplyByEarthRadius(Pi)
      else multiplyByEarthRadius(calculateCentralAngle(location1, location2))
    }

    greatCircleDistance(location1, location2)
  }

  // Alternative for Great Circle Distance
  // https://introcs.cs.princeton.edu/java/12types/GreatCircle.java.html
  /*
  def greatCircleDistanceByLawOfCosines(location1: Location, location2: Location): Double = {
    // Convert to radians
    val lat1 = math.toRadians(location1.lat)
    val long1 = math.toRadians(location1.lon)

    val lat2 = math.toRadians(location2.lat)
    val long2 = math.toRadians(location2.lon)

    // great circle distance in radians
    var angle1 = math.acos(math.sin(lat1) * math.sin(lat2) + math.cos(lat1) * math.cos(lat2) * math.cos(long1 - long2))

    // convert back to degrees
    angle1 = math.toDegrees(angle1)

    // each degree on a great circle of Earth is 60 nautical miles
    60 * angle1
  }
  */



  def sumWT(temps: Iterable[(Location, Temperature)], location: Location): Double =
    temps.map { case (loc, temp) => w(loc, location) * temp }.sum

  def sumW(temps: Iterable[(Location, Temperature)], location: Location): Double =
    temps.map { case (loc, _) => w(loc, location) }.sum

  def w(l1: Location, l2: Location): Double = {
    val dist = d(l1, l2)
    if (dist == 0.0) throw new Exception("Trying to get the weight of two locations that are the same")
    1 / (math.pow(dist, 2.0))
  }



  def findInterval(points: Iterable[(Temperature, Color)],
                   temperature: Temperature)
  : ((Temperature, Color), (Temperature, Color)) = {

    val pointsSorted = points.toSeq.sortBy(_._1)
    val pointsWithIndex: Seq[((Temperature, Color), Int)] = pointsSorted.zipWithIndex
    val upperBoundWithIndex: ((Temperature, Color), Int) = pointsWithIndex.find(i => i._1._1 >= temperature).get
    val uBoundIndex = upperBoundWithIndex._2
    val lowerBound = pointsSorted(uBoundIndex - 1)
    (lowerBound, upperBoundWithIndex._1)
  }



  def interpolateColorComponent(point1: (Temperature, Int), point2: (Temperature, Int), tempX: Temperature): Int = {
    val temp1 = point1._1
    val temp2 = point2._1
    val c1 = point1._2
    val c2 = point2._2

    if (temp2 == temp1) c1
    else Math.round((c1 * (temp2 - tempX) + c2 * (tempX - temp1)) / (temp2 - temp1)).toInt
  }



  def arrayIndexToPixelCoord(i: Int, imageWidth: Int): (Int, Int) = {
    val numRow: Int = (i / imageWidth)

    val x: Int = i - (numRow * imageWidth)
    val y: Int = numRow

    (x, y)
  }



  /* Version after adding tiles
   * (left for educationalPurposes)
   *
  def latLongToPixelCoord(location: Location, imageWidth: Int, imageHeight: Int): (Int, Int) = {
    val tile1: Tile = Tile(0, 0, 0)

    Interaction.latLongToPixelCoord(tile1, location, imageWidth, imageHeight)
  }
  // */

  /* Version before adding tiles
   * (left for educationalPurposes)
   */
  def latLongToPixelCoord(location: Location, imageWidth: Int, imageHeight: Int): (Int, Int) = {
    val ratioLongitudePixel = TOTAL_LONGITUDE_WIDTH / imageWidth
    val ratioLatitudePixel = TOTAL_LATITUDE_HEIGHT / imageHeight

    // takes into account edge case: longitude +180.0 (would fall off the array)
    val approxX: Double = math.min((location.lon + LONGITUDE_OFFSET) / ratioLongitudePixel, imageWidth - 1)

    // takes into account edge case: latitude +90.0 (would fall off the array)
    // bear in mind North->South latitudes decrease from +90 to -90  while Y's increase as they move Southwards
    val approxY: Double = math.min((math.abs(location.lat + (-LATITUDE_OFFSET)) / ratioLatitudePixel) , imageHeight - 1)

    (math.floor(approxX).toInt, math.floor(approxY).toInt)
  }
  // */




  /* Version after adding tiles
   * (left for educationalPurposes)
   *
  def pixelCoordToLatLong(x: Int, y: Int, imageWidth: Int, imageHeight: Int): Location = {
    val tile1: Tile = Tile(0, 0, 0)

    Interaction.pixelCoordToLatLong(tile1, x, y, imageWidth, imageHeight)
  }
  // */

  /*
   *  Version before adding tiles
   * (left for educationalPurposes)
   */
  def pixelCoordToLatLong(x: Int, y: Int, imageWidth: Int, imageHeight: Int): Location = {
    val ratioLongitudePixel = TOTAL_LONGITUDE_WIDTH / imageWidth
    val ratioLatitudePixel = TOTAL_LATITUDE_HEIGHT / imageHeight

    // bear in mind North->South latitudes decrease from +90 to -90  while Y's increase as they move Southwards
    val approxLat: Double = -((y * ratioLatitudePixel) - LATITUDE_OFFSET)

    val approxLong: Double = (x * ratioLongitudePixel) - LONGITUDE_OFFSET

    Location(approxLat, approxLong)
  }
  // */



  def pixelCoordToArrayIndex(x: Int, y: Int, imageWidth: Int): Int = x + (imageWidth * y)




  /* Version after adding tiles
   * (left for educationalPurposes)
   *
  def predictedTemperaturePixelColor(x: Int,
                                     y: Int,
                                     imageWidth: Int,
                                     imageHeight: Int,
                                     temperatures: Iterable[(Location, Temperature)],
                                     colors: Iterable[(Temperature, Color)]) : Pixel = {
    val tile1: Tile = Tile(0, 0, 0)

    Interaction.predictedTemperaturePixelColor(tile1, x, y, imageWidth, imageHeight, temperatures, colors)
  }
  // */

  /*
   *  Version before adding tiles
   * (left for educationalPurposes)
   */
  def predictedTemperaturePixelColor(x: Int,
                                     y: Int,
                                     imageWidth: Int,
                                     imageHeight: Int,
                                     temperatures: Iterable[(Location, Temperature)],
                                     colors: Iterable[(Temperature, Color)])
  : Pixel = {

    val loc: Location = pixelCoordToLatLong(x, y, imageWidth, imageHeight)
    val predictedTemp = predictTemperature(temperatures, loc)

    temperatureToPixelColor(predictedTemp, colors)
  }
  // */



  def temperatureToPixelColor(temperature: Temperature, colors: Iterable[(Temperature, Color)]): Pixel = {
    val predictedColor: Color = interpolateColor(colors, temperature)
    Pixel(predictedColor.red, predictedColor.green, predictedColor.blue, ALPHA)
  }



  /*
   *  Version after adding tiles
   * (left for educationalPurposes)
   *
  def calculateImage(temperatures: Iterable[(Location, Temperature)],
                     colors: Iterable[(Temperature, Color)],
                     imageWidth: Int,
                     imageHeight: Int):
  Image = {
    val tile1: Tile = Tile(0, 0, 0)

    Interaction.calculateImage(tile1, temperatures, colors, IMAGE_WIDTH, IMAGE_HEIGHT)
  }
  // */

  /*
   *  Version before adding tiles
   * (left for educationalPurposes)
   */
  def calculateImage(temperatures: Iterable[(Location, Temperature)],
                     colors: Iterable[(Temperature, Color)],
                     imageWidth: Int,
                     imageHeight: Int):
  Image = {

    val knownTemperaturesInPixelCoords: Iterable[((Int, Int), Temperature)] =
      temperatures.map(p => (latLongToPixelCoord(p._1, imageWidth, imageHeight), p._2))

    val knownTemperaturesByCoord: Map[(Int, Int), Temperature] = knownTemperaturesInPixelCoords.toMap

    // -----------
    // Parallel functional solution
    // -----------
    def temperatureForIndex(i: Int): Pixel = {
      val coord: (Int, Int) = arrayIndexToPixelCoord(i, imageWidth)
      if (knownTemperaturesByCoord.contains(coord))
        temperatureToPixelColor(knownTemperaturesByCoord(coord), colors)
      else
        predictedTemperaturePixelColor(coord._1, coord._2, imageWidth, imageHeight, temperatures, colors)
    }

    val pixelArrayPar: ParArray[Pixel] = new ParArray[Pixel](imageWidth * imageHeight)
    val pixelArrayOutput: Array[Pixel] =
      pixelArrayPar.zipWithIndex.map(p => (temperatureForIndex(p._2))).arrayseq.toArray

    Image(imageWidth, imageHeight, pixelArrayOutput)


          // -----------
          // Non-parallel imperative solution
          // -----------
          /*
          val pixelArraySize = imageWidth * imageHeight
          val pixelArray: Array[Pixel] = new Array[Pixel](pixelArraySize)
          for (i <- 0 until pixelArraySize) {
            val coord: (Int, Int) = arrayIndexToPixelCoord(i, imageWidth)
            if (knownTemperaturesByCoord.contains(coord))
              pixelArray(i) =  temperatureToPixelColor(knownTemperaturesByCoord(coord), colors)
            else
              pixelArray(i) = predictedTemperaturePixelColor(coord._1, coord._2, imageWidth, imageHeight, temperatures, colors)
          }
          Image(imageWidth, imageHeight, pixelArray)
          // */

  }
  // */
}
