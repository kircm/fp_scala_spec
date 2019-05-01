package observatory

import scala.collection.mutable

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    // Mutable Map to speed things up when building function memoizations
    var tmpTemperaturesByGridLocation: mutable.Map[GridLocation, Temperature] = mutable.Map()

    for (latGrid: Int <- -89 to 90) {
      for (longGrid: Int <- -180 to 179) {
        val gridLocation = GridLocation(latGrid, longGrid)
        val temperatureAtGridLocation = Visualization.predictTemperature(temperatures, Location(latGrid.toDouble, longGrid.toDouble))

        tmpTemperaturesByGridLocation += (gridLocation -> temperatureAtGridLocation)
      }
    }

    // Immutable map
    val temperaturesByGridLocation: Map[GridLocation, Temperature] = tmpTemperaturesByGridLocation.toMap

    def temperatureAtGridLocationFn(gridLocation: GridLocation): Temperature = {
      temperaturesByGridLocation(gridLocation)
    }

    temperatureAtGridLocationFn
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val numYears = temperaturess.size

    // Mutable Map to speed things up when building function memoizations
    var tmpAverageTemperaturesByGridLocation: mutable.Map[GridLocation, Temperature] = mutable.Map()

    val grids: Iterable[GridLocation => Temperature] = temperaturess.map(t => makeGrid(t))

    for (latGrid: Int <- -89 to 90) {
      for (longGrid: Int <- -180 to 179) {
        val gridLocation = GridLocation(latGrid, longGrid)

        // parallel execution to sum all temperatures for the current gridLocation across all grids (years)
        //val sumTempAtGridLocation: Temperature = grids
        //  .par.aggregate(0.0)((acc: Double, f: GridLocation => Temperature) => f(gridLocation) + acc, _ + _)

        // sequential execution to sum all temperatures for the current gridLocation across all grids (years)
        val sumTempAtGridLocation: Temperature = grids
          .aggregate(0.0)((acc: Double, f: GridLocation => Temperature) => f(gridLocation) + acc, _ + _)

        val avgTempAtGridLocation: Temperature = sumTempAtGridLocation / numYears
        tmpAverageTemperaturesByGridLocation += (gridLocation -> avgTempAtGridLocation)
      }
    }

    // Immutable map
    val averageTemperaturesByGridLocation: Map[GridLocation, Temperature] = tmpAverageTemperaturesByGridLocation.toMap

    def avgTemperatureAtGridLocationFn(gridLocation: GridLocation): Temperature = {
      averageTemperaturesByGridLocation(gridLocation)
    }

    avgTemperatureAtGridLocationFn
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    // Mutable Map to speed things up when building function memoizations
    var tmpDeviationsByGridLocation: mutable.Map[GridLocation, Temperature] = mutable.Map()

    val gridKnownTemperatures: GridLocation => Temperature = makeGrid(temperatures)

    for (latGrid: Int <- -89 to 90) {
      for (longGrid: Int <- -180 to 179) {
        val gridLocation = GridLocation(latGrid, longGrid)
        val deviationAtGridLocation = gridKnownTemperatures(gridLocation) - normals(gridLocation)
        tmpDeviationsByGridLocation += (gridLocation -> deviationAtGridLocation)
      }
    }

    // Immutable map
    val deviationsByGridLocation: Map[GridLocation, Temperature] = tmpDeviationsByGridLocation.toMap

    def deviationAtGridLocationFn(gridLocation: GridLocation): Temperature = {
      deviationsByGridLocation(gridLocation)
    }

    deviationAtGridLocationFn
  }


}

