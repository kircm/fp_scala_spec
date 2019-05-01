package water.pouring.marc

import scala.collection.mutable

class Pouring(capacities: mutable.ArrayBuffer[Int], states: mutable.ArrayBuffer[Int]) {

  def fillUp(glass: Int): Boolean = {
    if (states(glass) == capacities(glass)) return false
    states(glass) = capacities(glass)
    printMe
    return true
  }

  def empty(glass: Int): Boolean = {
    if (states(glass) == 0) return false
    states(glass) = 0
    printMe
    return true
  }

  def pourInto(fromGlass: Int, toGlass: Int): Boolean = {
    if (pourIntoAux(fromGlass, toGlass)) {
      printMe
      return true
    } else return false
  }

  def printMe = print("states: " + states)

  def getStates = states

  def nextMoves: List[() => Boolean] = ???



  /*
      Private methods
   */
  private def pourIntoAux(fromGlass: Int, toGlass: Int): Boolean = {
    if (states(fromGlass) == 0) return false
    if (states(toGlass) == capacities(toGlass)) return false

    if (remaining(toGlass) < states(fromGlass)) {
      val transfer = remaining(toGlass)
      states(fromGlass) = states(fromGlass) - transfer
      states(toGlass) = capacities(toGlass) // same as  states(toGlass) + transfer
      return true
    }

    if (remaining(toGlass) >= states(fromGlass)) {
      states(toGlass) = states(toGlass) + states(fromGlass)
      states(fromGlass) = 0
      return true
    }

    return false
  }

  private def remaining(inGlass: Int): Int = {
    if (capacities(inGlass) < states(inGlass)) throw new RuntimeException("impossible state")
    if (capacities(inGlass) == states(inGlass)) return 0
    capacities(inGlass) - states(inGlass)
  }
}

