package water.pouring.coursera

class Pouring(capacity: Vector[Int]) {

  // States

  type State = Vector[Int]

  val initialState = capacity map (x => 0)


  // Moves

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state map (x => if (x == glass) 0 else x)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state map (x => if (x == glass) capacity(glass) else x)
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated (to, state(to) + amount)
    }
  }


  // generate glasses and possible moves

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))


  // Paths

  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState) (_ change _)

    /* method endState using pattern matching and recursive calls instead of foldRight, for clarity:
    def endState: State = trackState(history)

    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialState
      case move :: xs1 => move change trackState(xs1)
    }
    */

    def extend(move: Move) = new Path(move :: history)

    override def toString = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil)

  // Very inneficient!! It may go into cycles of moves that visit already explored states
  def from(paths: Set[Path]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
      } yield next
      paths #:: from(more)
    }

  val pathSets = from(Set(initialPath))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
