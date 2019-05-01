package observatory

import java.time.LocalDate

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.log4j.{Level, Logger}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val sc: SparkContext = getSparkContext
    val stations: RDD[String]  = sc.textFile(getClass.getResource(stationsFile).toString)
    val temperatures: RDD[String]  = sc.textFile(getClass.getResource(temperaturesFile).toString)

    val st = stations.filter(s => s.split(",").size == 4).map(s => parseStation(s))
    val tmp = temperatures.map(t => parseTemperature(t))

    val joined = st.join(tmp)
    val projected = joined.values.map(v => (LocalDate.of(year, v._2._1, v._2._2), v._1, fahrenheitToCelsiusTemperature(v._2._3)))

    projected.collect
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val sc: SparkContext = getSparkContext

    val recsRdd: RDD[(LocalDate, Location, Temperature)] = sc.parallelize(records.toSeq)

    val pairRdd: RDD[((Year, Location), Temperature)] = recsRdd.map(r => ((r._1.getYear, r._2), r._3))

    val intermediate: RDD[((Year, Location), (Temperature, Year))] =
      pairRdd
        .mapValues(p => (p, 1))
        .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))

    val avgTemps: RDD[((Year, Location), Temperature)] = intermediate.mapValues{
      case (tempSum, numEvents) => tempSum / numEvents
    }

    avgTemps.map(p => (p._1._2, p._2)).collect
  }

  private def getSparkContext: SparkContext = {
    val sparkSession = SparkSession
      .builder()
      .master("local")
      .appName("observatory")
      .getOrCreate()
    sparkSession.sparkContext
  }

  private def parseStation(line: String): ((String, String), Location) = {
    val lineData = line.split(",")
    ((lineData(0), lineData(1)), Location(lineData(2).toDouble, lineData(3).toDouble))
  }

  private def parseTemperature(line: String): ((String, String), (Int, Int, Double)) = {
    val lineData = line.split(",")
    ((lineData(0), lineData(1)), (lineData(2).toInt, lineData(3).toInt, lineData(4).toDouble))
  }

  private def fahrenheitToCelsiusTemperature(d: Double): Temperature = (d - 32) * 5 / 9

}
