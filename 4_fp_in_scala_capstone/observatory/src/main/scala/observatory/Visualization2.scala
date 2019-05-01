package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = unitSquare(point, d00, d01, d10, d11)

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    ???
  }


  /* ------------------------------
     ----- Added Functions -------
     ------------------------------ */

  def unitSquare(point: CellPoint,
                 d00: Temperature,
                 d01: Temperature,
                 d10: Temperature,
                 d11: Temperature): Temperature = {

    // https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_square

    val x = point.x
    val y = point.y

    val component1 = d00 * (1 - x) * (1 - y)
    val component2 = d10 * x * (1 - y)
    val component3 = d01 * (1 - x) * y
    val component4 = d11 * x * y

    component1 + component2 + component3 + component4
  }


}
