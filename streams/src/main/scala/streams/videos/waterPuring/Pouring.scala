package streams.videos.waterPuring


class Pouring(capacity: Vector[Int]) {

  //State
  type State = Vector[Int]
  val initialState: State = capacity map (_ => 0)

  //moves
  trait Moves {
    def change(state: State): State
  }

  case class Fill(n: Int) extends Moves {
    def change(state: State): State = state.updated(n, capacity(n))
  }

  case class Empty(n: Int) extends Moves {
    def change(state: State): State = state.updated(n, 0)
  }

  case class proudOn(cup1: Int, cup2: Int) extends Moves {
    def change(state: State): State = {
      val amount = state(cup1) min (capacity(cup2) - state(cup2)) // capacidad - cantidad de agua actual

      state updated(cup1, state(cup1) - amount) updated(cup2, state(cup2) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves: IndexedSeq[Moves] =
    (for (glass <- glasses) yield Fill(glass)) ++
      (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass1 <- glasses; glass2 <- glasses if glass1 != glass2) yield proudOn(glass1, glass2))

  //Path
  class Path(history: List[Moves], val endState: State) {
    //def endState: State = (history foldRight initialState) (_ change _)

    def extend(move: Moves): Path = new Path(move :: history, move.change(endState))

    override def toString: String = (history.reverse mkString " ") + " --> " + endState

    //Dado una lista de movimientos y un estado final saber el estado final
    /*    def endState: State = trackState(history)

        private def trackState(value: List[Moves]): State = value match {
          case move :: xi => move change trackState(xi)
          case Nil => initialState
        }*/
  }

  val initialPath = new Path(Nil,initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more: Set[Path] = for (path <- paths;
                                 next: Path <- moves.map(move => path.extend(move));
                                 if !explored.contains(next.endState)) //trabajar con paths en vez de con move
        yield next

      val setEndState: Set[State] = more map (_.endState)

      paths #:: from(more, explored ++ setEndState)
    }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  //Stream con todos los path que finalicen con una solucion ordenadas de menor a mayor

  def solution(target: Int): Stream[Path] =
    for {pathSet <- pathSets
         path <- pathSet
         if path.endState contains target}
      yield path
}
