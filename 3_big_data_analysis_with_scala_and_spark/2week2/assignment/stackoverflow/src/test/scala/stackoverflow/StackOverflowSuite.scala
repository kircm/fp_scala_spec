package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import stackoverflow.StackOverflow.{groupedPostings, rawPostings, scoredPostings, vectorPostings, sc}

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }


  // MK
  test("group postings one question") {
    val path = getClass.getResource("/01_test_stackoverflow.csv").getPath
    val grouped: Array[(QID, Iterable[(Question, Answer)])] = getGroupedFromFile(path).collect

    grouped.foreach(p => println(p))

    assert(grouped.size == 1, "There should be 1 group")
  }

  // MK
  test("group postings > 1 question") {
    val path = getClass.getResource("/02_test_stackoverflow.csv").getPath
    val grouped: Array[(QID, Iterable[(Question, Answer)])] = getGroupedFromFile(path).collect

    grouped.foreach(p => println(p))

    assert(grouped.size == 2, "There should be 2 groups")
  }

  // MK
  test("score postings one question") {
    val path = getClass.getResource("/01_test_stackoverflow.csv").getPath
    val groupedRdd: RDD[(QID, Iterable[(Question, Answer)])] = getGroupedFromFile(path)
    val scoredRdd: RDD[(Question, HighScore)] = scoredPostings(groupedRdd)

    val scoredCollected = scoredRdd.collect
    scoredCollected.foreach(p => println(p))

    assert(scoredCollected.size == 1, "There should be 1 scored question")
  }

  // MK
  test("score postings two questions") {
    val path = getClass.getResource("/02_test_stackoverflow.csv").getPath
    val groupedRdd: RDD[(QID, Iterable[(Question, Answer)])] = getGroupedFromFile(path)
    val scoredRdd: RDD[(Question, HighScore)] = scoredPostings(groupedRdd)

    val scoredCollected = scoredRdd.collect
    scoredCollected.foreach(p => println(p))

    assert(scoredCollected.size == 2, "There should be 2 scored questions")
  }

  // MK
  test("vector of postings for two questions") {
    val path = getClass.getResource("/02_test_stackoverflow.csv").getPath
    val groupedRdd: RDD[(QID, Iterable[(Question, Answer)])] = getGroupedFromFile(path)
    val scoredRdd: RDD[(Question, HighScore)] = scoredPostings(groupedRdd)
    val vectorRdd: RDD[(LangIndex, HighScore)] = vectorPostings(scoredRdd)

    val vectorCollected = vectorRdd.collect
    vectorCollected.foreach(v => println(v))

    assert(vectorCollected.size == 2, "Vector should contain 2 data points")
  }

  // MK
  test("Calculate median even number of scores") {
    val highScores: Array[HighScore] = Array(1, 8, 4, 9, 10, 3, 3, 4)
    val median: Double = StackOverflow.calculateMedian(highScores)

    assert(median.toInt == 4)
  }

  // MK
  test("Calculate median odd number of scores") {
    val highScores: Array[HighScore] = Array(1, 8, 3, 9, 10, 3, 3, 4, 3)
    val median: Double = StackOverflow.calculateMedian(highScores)
    assert(median.toInt == 3)
  }

  // MK
  private def getGroupedFromFile(path: String) = {
    val lines = sc.textFile(path)
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    grouped
  }
}
