package observatory

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object Utils {

  def dumpImageBytesToFile(year: Year, tileZoom: Int, tileX: Int, tileY: Int, image: BufferedImage): Unit = {
    val path: String = s"./target/temperatures/$year/$tileZoom/$tileX-$tileY.png"

    println(s"Generating image file: $path")

    val f: File = new File(path)
    f.mkdirs
    ImageIO.write(image, "PNG", f)
  }

}
