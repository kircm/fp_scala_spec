package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1 */


    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin



    /* Easier level: List(Right, Down, Left)
    val level =
      """ooo-------
        |oSoooo----
        |oTooooooo-
        |-ooooooooo
        |-----ooooo
        |------ooo-""".stripMargin
     */

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("isStanding") {
    new Level1 {
      assert(startBlock.isStanding)
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("neighbors with history") {
    new Level1 {
      val neighbors = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val sortedVector = neighbors.toVector.sortBy(_._1.b1.row)

      assert(sortedVector.size == 2)
      assert(sortedVector.head._1 == Block(Pos(1, 2), Pos(1, 3)))
      assert(sortedVector.head._2 == List(Right, Left, Up))

      assert(sortedVector(1)._1 == Block(Pos(2, 1), Pos(3, 1)))
      assert(sortedVector(1)._2 == List(Down, Left, Up))
    }
  }

  test("neighbors without cycles") {
    new Level1 {
      val visitedBlock = Block(Pos(1, 2), Pos(1, 3))
      val visitedBlock2 = Block(Pos(1, 1), Pos(1, 1))
      val unvisitedBlock = Block(Pos(2, 1), Pos(3, 1))

      val neighborsWithHistory = (visitedBlock, List(Right, Left, Up)) #:: (unvisitedBlock, List(Down, Left, Up)) #:: Stream.empty
      val explored = Set(visitedBlock, visitedBlock2)

      val newNeighbors = newNeighborsOnly(neighborsWithHistory, explored).toVector

      assert(newNeighbors.size == 1)
      assert(newNeighbors.head._1 == unvisitedBlock)
    }
  }

  test("all possible paths from") {
    new Level1 {
      val block1 = Block(Pos(1, 2), Pos(1, 3))
      val block2 = Block(Pos(2, 1), Pos(3, 1))

      val path1 = (block1, List[Move](Right))
      val path2 = (block2, List[Move](Down))

      val initialStreamOfPaths = path1 #:: path2 #:: Stream.empty

      val exploredBlock1 = Block(Pos(1, 1), Pos(1, 1))
      val exploredBlocks = Set(exploredBlock1)

      val pathsFrom = from(initialStreamOfPaths, exploredBlocks)

      val newBlock1 = Block(Pos(2, 2), Pos(3, 2))
      val newBlock2 = Block(Pos(1, 4), Pos(1, 4))
      val newBlock3 = Block(Pos(2, 2), Pos(2, 3))

      assert(pathsFrom.size == 3)
      assert(pathsFrom.toVector(0)._1 == newBlock1)
      assert(pathsFrom.toVector(0)._2 == List(Right, Down))

      assert(pathsFrom.toVector(1)._1 == newBlock2)
      assert(pathsFrom.toVector(1)._2 == List(Right, Right))

      assert(pathsFrom.toVector(2)._1 == newBlock3)
      assert(pathsFrom.toVector(2)._2 == List(Down, Right))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }


}
