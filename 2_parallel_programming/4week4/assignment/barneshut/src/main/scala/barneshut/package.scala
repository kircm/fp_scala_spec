import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, List(b))
  }

  // MK: defined trait Quadrant and case objects for convenience, used as enum
  trait Quadrant
  case object NW extends Quadrant
  case object NE extends Quadrant
  case object SW extends Quadrant
  case object SE extends Quadrant

  //MK: precision threshold for floating point comparisons
  val precisionThreshold = 1e-4

  // MK: float comparison with precision threshold
  private def floatBiggerThanFloatWithinPrecision(floatLeft: Float, floatRight: Float): Boolean =
    floatLeft >= floatRight && (floatLeft - floatRight).abs > precisionThreshold


  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = bottomLeftCornerX(nw)

    val centerY: Float = bottomLeftCornerY(nw)

    val size: Float = nw.size * 2.0f

    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass

    val massX: Float =
      if (mass == 0) centerX
      else (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass

    val massY: Float =
      if (mass == 0) centerY
      else (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / mass

    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      println(s"\nFork (${centerX}, ${centerY}, ${size}).insert() Body: " + b )

      val quadToInsert: (Quadrant, Quad) = findQuadrantForNewBody(b)
      println("Fork.insert() quadToInsertTo: " + quadToInsert)

      val newQuad: Quad = quadToInsert._2.insert(b)
      println("Fork.insert() newQuad: " + newQuad)

      val newFork: Fork = quadToInsert._1 match {
        case NW => new Fork(newQuad, ne, sw, se)
        case NE => new Fork(nw, newQuad, sw, se)
        case SW => new Fork(nw, ne, newQuad, se)
        case SE => new Fork(nw, ne, sw, newQuad)
      }

      println("Fork.insert() newForkWithNewQuad: " + newFork)
      newFork
    }

    def insert(bodies: Seq[Body]): Fork = bodies match {
      case Nil => this
      case (bodyHead :: bodiesTail) => this.insert(bodyHead).insert(bodiesTail)
    }

    private def bottomLeftCornerX(q: Quad): Float = q.centerX + (q.size / 2.0f)

    private def bottomLeftCornerY(q: Quad): Float = q.centerY + (q.size / 2.0f)

    private def findQuadrantForNewBody(b: Body): (Quadrant, Quad) = {
      val halfQuadrantSize = nw.size / 2

      val minX = nw.centerX - halfQuadrantSize
      val maxX = se.centerX + halfQuadrantSize
      val minY = nw.centerY - halfQuadrantSize
      val maxY = se.centerY + halfQuadrantSize
      assert(b.x >= minX, s"Body's X ${b.x} smaller than minX ${minX}")
      assert(b.x <= maxX, s"Body's X ${b.x} bigger than maxX ${maxX}")
      assert(b.y >= minY, s"Body's Y ${b.y} smaller than minY ${minY}")
      assert(b.y <= maxY, s"Body's Y ${b.y} bigger than maxY ${maxY}")

      val nwMaxX = nw.centerX + halfQuadrantSize
      val nwMaxY = nw.centerY + halfQuadrantSize

      val east = b.x > nwMaxX
      val south = b.y > nwMaxY

      (east, south) match {
        case (false, false) => (NW, nw)
        case (true, false) => (NE, ne)
        case (false, true) => (SW, sw)
        case (true, true) => (SE, se)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body]) extends Quad {

    val (mass: Float, massX: Float, massY: Float) = (
      bodiesTotalMass(bodies),
      massCenterForCoord(coordXFn, bodies),
      massCenterForCoord(coordYFn, bodies))

    val total: Int = bodies.size

    def insert(b: Body): Quad = {
      println(s"\nLeaf (${centerX}, ${centerY}, ${size}).insert() b: " + b)
      println("Leaf.insert() size: " + size)

      if (floatBiggerThanFloatWithinPrecision(this.size, minimumSize)) {
        val newQuadsSize: Float = this.size / 2
        val newQuadsOffset: (Float, Float) = (offset(this.centerX, this.size), offset(this.centerY, this.size))
        println("Leaf.insert() newQuadsSize: " + newQuadsSize)
        println("Leaf.insert() newQuadsOffset: " + newQuadsOffset)

        val newFork: Fork = createForkForBodies(newQuadsSize, newQuadsOffset)
        println("Leaf.insert() newFork: " + newFork)

        newFork.insert(bodies :+ b)
      }
      else {
        new Leaf(centerX, centerY, size, bodies :+ b)
      }
    }

    private def offset(c: Float, size: Float): Float = c - (size / 2)

    // MK: the functions to be passed in as parameters to be called inside the aggregate that
    // calculates mass center for coordinate X and for Y
    // This is for code re-usability between X and Y calculations
    private def coordXFn = ((b: Body) => b.x)

    private def coordYFn = ((b: Body) => b.y)

    // MK: Using reduce limits to sequential calculation:
    // - def bodiesTotalMass: Float = bodies.reduce((b1, b2) => b1.mass + b2.mass)
    //
    // Using aggregate
    // - aggregate: allows for parallel computation
    // - Seq[Body] is a trait that is subtype of GenSeq[Body] so it defines aggregate
    private def bodiesTotalMass(bodies: Seq[Body]): Float =
      bodies.aggregate(0f)({ (acc, body) => acc + body.mass}, { (m1, m2) => m1 + m2})


    // Using aggregate here as well
    private def massCenterForCoord(coordFn: ((Body) => Float), bodies: Seq[Body]): Float =
      bodies.aggregate(0f)(
        {
          (acc, body) => acc + (body.mass * coordFn(body))
        }, {
          (m1, m2) => m1 + m2
        })  / bodiesTotalMass(bodies)


    private def createForkForBodies(newQuadsSize: Float, newQuadsOffset: (Float, Float)): Fork = {
      val xOffset = newQuadsOffset._1
      val yOffset = newQuadsOffset._2

      val westCenterX = xOffset + (newQuadsSize / 2)
      val northCenterY = yOffset + newQuadsSize / 2
      val eastCenterX = xOffset + newQuadsSize + (newQuadsSize / 2)
      val southCenterY = yOffset + newQuadsSize + (newQuadsSize / 2)

      val nw: Empty = Empty(westCenterX, northCenterY, newQuadsSize)
      val ne: Empty = Empty(eastCenterX, northCenterY, newQuadsSize)
      val sw: Empty = Empty(westCenterX, southCenterY, newQuadsSize)
      val se: Empty = Empty(eastCenterX, southCenterY, newQuadsSize)

      new Fork(nw, ne, sw, se)
    }
  }

  // original size
  def minimumSize = 0.00001f
  // test size
  //def minimumSize = 1f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  case class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force

        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.map(thatBody => addForce(thatBody.mass, thatBody.x, thatBody.y))

        case Fork(nw, ne, sw, se) => {
          // see if node is far enough from the body,
          // or recursion is needed
          val dist = distance(quad.centerX, quad.centerY, x, y)
          if (floatBiggerThanFloatWithinPrecision(theta, quad.size / dist)) addForce(quad.mass, quad.centerY, quad.centerY)
          else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
        }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val cb: ConcBuffer[Body] = findSectorForBody(b)
      cb += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val combinedMatrix = new SectorMatrix(this.boundaries, this.sectorPrecision)
      val matrixSize = this.sectorPrecision * this.sectorPrecision

      for (i <- 0 until matrixSize) {
          val combinedSector: ConcBuffer[Body] = this.matrix(i).combine(that.matrix(i))
          combinedMatrix.matrix(i) = combinedSector
      }

      combinedMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    private def findSectorForBody(body: barneshut.Body): ConcBuffer[Body] = {
      val bodyRelativeX = body.x - boundaries.minX
      val bodyRelativeY = body.y - boundaries.minY
      val chosenSectorX: Int = (Math.floor(bodyRelativeX / sectorSize)).asInstanceOf[Int]
      val chosenSectorY: Int = (Math.floor(bodyRelativeY / sectorSize)).asInstanceOf[Int]

      println(s"chosenSectorX: ${chosenSectorX}")
      println(s"chosenSectorY: ${chosenSectorY}")

      this(chosenSectorX, chosenSectorY)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
