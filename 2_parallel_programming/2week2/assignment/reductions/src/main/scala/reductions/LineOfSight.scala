package reductions

import org.scalameter._
import common._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def currentTan(heights: Array[Float], distance: Int): Float = heights(distance) / distance

  def scanFunctionMaxTan(heights: Array[Float], currentMaxTan: Float, distance: Int) = {
    max(currentMaxTan, currentTan(heights, distance))
  }

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    if (input.length > output.length) throw new RuntimeException("Output array too small")
    if (input.length < 1) throw new RuntimeException("Empty Array")

    val a0: Float = 0.0F
    output(0) = a0

    var currentMaxTan = a0
    for (i <- 1 until input.length) {
      currentMaxTan = scanFunctionMaxTan(input, currentMaxTan, i)
      output(i) = currentMaxTan
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
    */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var maxTan = 0.0F
    for (i <- from until until) {
      maxTan = scanFunctionMaxTan(input, maxTan, i)
    }

    return maxTan
  }

  def calculateMiddleIndex(from: Int, end: Int): Int = {
    from + (end - from) / 2
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
    * returns the reduction tree for that part of the array.
    *
    * The reduction tree is a `Leaf` if the length of the specified part of the
    * array is smaller or equal to `threshold`, and a `Node` otherwise.
    * If the specified part of the array is longer than `threshold`, then the
    * work is divided and done recursively in parallel.
    */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if (end - from <= threshold) {
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      val mid: Int = calculateMiddleIndex(from, end)
      val trees: (Tree, Tree) = parallel(
        upsweep(input, from, mid, threshold),
        upsweep(input, mid, end, threshold))

      Node(trees._1, trees._2)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
    * `until`, and computes the maximum angle for each entry of the output array,
    * given the `startingAngle`.
    */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {

    var maxAngle: Float = startingAngle
    for (i <- from until until) {
      maxAngle = scanFunctionMaxTan(input, maxAngle, i)
      output(i) = maxAngle
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
    * reduction `tree` in parallel, and then calls `downsweepSequential` to write
    * the `output` angles.
    */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = tree match {
    case Leaf(from, until, maxPrevious) => {
      /*
      println(s"-------- Leaf start  --- startingAngle: $startingAngle")
      println(s"Leaf: $from $until $maxPrevious")
      println(s"Calling downsweepSequential(input, output, $startingAngle, $from, $until)")
       */

      downsweepSequential(input, output, startingAngle, from, until)

      /*
      println("-------- Leaf end ")
       */
    }
    case Node(left, right) => {
      val leftStartingAngle = startingAngle
      val rightStartingAngle = max(startingAngle, left.maxPrevious)

      // Sequential execution with output for debugging
      /*
      println(s"-------- Node start --- startingAngle: $startingAngle")
      println(s"Node: maxPrevious(${tree.maxPrevious})")
      println(s"leftStartingAngle: $leftStartingAngle ")
      println(s"rightStartingAngle: $rightStartingAngle ")

      println(s"Calling downsweep(input, output, $leftStartingAngle, left)")
      downsweep(input, output, leftStartingAngle, left)

      println(s"Calling downsweep(input, output, $rightStartingAngle, right)")
      downsweep(input, output, rightStartingAngle, right)

      println("-------- Node end ")
      */

      // Parallel execution without output
      /* */
      parallel(
        downsweep(input, output, leftStartingAngle, left),
        downsweep(input, output, rightStartingAngle, right)
      )
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
                     threshold: Int): Unit = {

    val upsweepTree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0F, upsweepTree)
  }
}
