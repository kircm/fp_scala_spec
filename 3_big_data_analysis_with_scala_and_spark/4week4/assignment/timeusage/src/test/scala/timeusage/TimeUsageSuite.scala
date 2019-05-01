package timeusage

import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("define shema") {
    val columnNames = List("Column1", "Column2", "Column3", "Column4")
    val schema = TimeUsage.dfSchema(columnNames)

    println(schema)
    assert(schema.head.dataType == StringType)
    assert(schema.tail.head.dataType == DoubleType)
    assert(schema.tail.tail.head.dataType == DoubleType)
  }

  test("row") {
    val line = List("SomeString", "1.0", "2.0", "3.0")
    val row = TimeUsage.row(line)

    assert(row.size == 4)
    assert(row.getString(0) == "SomeString")
    assert(row.getDouble(1) == 1.0)
    assert(row.getDouble(2) == 2.0)
    assert(row.getDouble(3) == 3.0)
  }


  test("classifiedColumns") {
    val classified: (List[Column], List[Column], List[Column]) =
      TimeUsage.classifiedColumns(List("t01Column", "t03Column", "t1805Column", "t18Column", "t06Column"))

    assert(classified._1.length == 2)
    assert(classified._1(0) == new Column("t01Column"))
    assert(classified._1(1) == new Column("t03Column"))

    assert(classified._2.length == 1)
    assert(classified._2(0) == new Column("t1805Column"))

    assert(classified._3.length == 2)
    assert(classified._3(0) == new Column("t18Column"))
    assert(classified._3(1) == new Column("t06Column"))
  }



}
