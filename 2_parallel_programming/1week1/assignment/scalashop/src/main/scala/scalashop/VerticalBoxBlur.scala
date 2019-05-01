package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scala.collection.parallel.Task

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (y <- src.height - 1 to 0 by -1) {
      for (x <- from until end) {
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
      }
    }
  }

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int, numTask: Int): Unit = {
    val lastPixel = end - 1
    println(s"-> VerticalBoxBlur.blur() task number: $numTask - Pixel columns from $from to $lastPixel")
    blur(src, dst, from, end, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    /*  Using a mutable ListBuffer
    val pixelsPerSection = src.width / numTasks
    val tasks = scala.collection.mutable.ListBuffer.empty[ForkJoinTask[Unit]]
    var currentTask = 1
    for (currentTaskFrom <- 0 to (dst.width - 1) by pixelsPerSection) {
      val currentTaskTo = currentTaskFrom + pixelsPerSection
      tasks.append(task(blur(src, dst, currentTaskFrom, currentTaskTo, radius)))
      println(s"Appended Task number: $currentTask")
      currentTask += 1
    }
    tasks.map(_.join)
    */

    /* Using zip */
    val totalPixelColumns: Int = src.width
    val pixelsPerSection: Int = totalPixelColumns / numTasks
    println(s"Num tasks: $numTasks")
    println(s"Total pixel columns: $totalPixelColumns")
    println(s"Pixels per section: $pixelsPerSection")

    (1 to numTasks)
      .zip(0 until totalPixelColumns by pixelsPerSection)
      .map { case (numTask, currentPixelFrom) =>
        if (numTask < numTasks)
          task(blur(src, dst, currentPixelFrom, currentPixelFrom + pixelsPerSection, radius, numTask))
        else
          task(blur(src, dst, currentPixelFrom, src.width, radius, numTask))
      }
      .map(_.join)
  }
}
