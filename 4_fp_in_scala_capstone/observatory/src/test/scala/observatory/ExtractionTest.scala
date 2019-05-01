package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  test("test locateTemperatures") {
    val output = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")

    val temp1202 = output.filter(o => o._1.equals(LocalDate.of(2015, 12, 2)) && o._2.equals(Location(67.1, -157.85))).head

    assert(output.size == 4)
    assert(temp1202._3 == 0.0d)
  }

  test("test locateTemperatures stations with no location are ignored") {
    val output = Extraction.locateTemperatures(2015, "/stations_to_ignore.csv", "/2015_to_ignore.csv")
    assert(output.size == 2)
  }

  test("test locationYearlyAverageRecords") {
    val locateTemperatures = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val yearlyAverages = Extraction.locationYearlyAverageRecords(locateTemperatures)

    assert(yearlyAverages.size == 1)
    assert(yearlyAverages.head._2 == 5.0)
  }
}