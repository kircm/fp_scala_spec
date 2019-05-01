package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle array of 1 element") {
    val res = upsweepSequential(Array[Float](0f), 1, 1)
    assert(res == 0f)
  }

  test("upsweepSequential should correctly handle array of 2 element") {
    val res = upsweepSequential(Array[Float](0f, 1f), 1, 2)
    assert(res == 1f)
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep parallel should build tree with intermediate results") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 3f, 3f, 3f, 3f, 3f, 3f, 3f, 55f), 1, 12, 6)
    assert(res.maxPrevious == 5f)
  }

  test("upsweep parallel should build tree on the indices 1 until 5 of a 5 element array for threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 3f), 1, 5, 1)
    assert(res.maxPrevious == 5f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  def printUpsweepTree(tree: Tree): Unit = {
    println("upsweepTree (tree built by upsweep process): \n")
    printTree(tree)
  }

  def printTree(tree: Tree): Unit = tree match {
    case Leaf(from, until, maxPrevious) => println(s"Leaf from($from) until($until) maxPrevious($maxPrevious)")
    case Node(left, right) => {
      println(s"Node - maxPrevious: ${tree.maxPrevious}")
      printTree(left)
      printTree(right)
    }
  }

  test("downsweep parallel should build tree with results in leafs for input arry of lentgth 4") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](input.length)
    val upsweepTree = upsweep(input, 1, 4, 2)

    printUpsweepTree(upsweepTree)

    downsweep(input, output, 0F, upsweepTree)

    val expected = List(0f, 1f, 4f, 4f)
    assert(output.toList == expected)
  }

  test("downsweep parallel should build tree with results in leafs for input arry of lentgth 12") {
    val input = Array[Float](0f, 1f, 8f, 9f, 3f, 3f, 3f, 3f, 3f, 3f, 3f, 55f)
    val output = new Array[Float](input.length)
    val upsweepTree = upsweep(input, 1, 12, 2)

    printUpsweepTree(upsweepTree)

    downsweep(input, output, 0F, upsweepTree)

    val expected = List(0.0, 1.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 5.0)
    assert(output.toList == expected)
  }

  test("parLineOfSight should compute line-of-sight") {
    val input = Array[Float](0f, 1f, 8f, 9f, 3f, 3f, 3f, 3f, 3f, 3f, 3f, 55f)
    val output = new Array[Float](input.length)

    parLineOfSight(input, output, 2)

    val expected = List(0.0, 1.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 5.0)
    assert(output.toList == expected)
  }
}

