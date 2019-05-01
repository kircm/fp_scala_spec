package observatory

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object TestUtils {

  def assertWithMarginOfError(actual: Double, expected: Double): Unit = assertWithMarginOfError(actual, expected, 5.0)

  def assertWithMarginOfError(actual: Double, expected: Double, margin: Double): Unit =
    assert(math.abs(actual - expected) <= margin)


  def dumpImageBytesToFile(tileZoom: Int, tileX: Int, tileY: Int, image: BufferedImage): Unit = {
    val path: String = s"./test_output/$tileZoom/$tileX/$tileY.jpeg"

    print(s"Generating image file: $path")

    /* do not generate test image files during grading
    val f: File = new File(path)
    f.mkdirs
    ImageIO.write(image, "JPEG", f)
    */
  }
}
