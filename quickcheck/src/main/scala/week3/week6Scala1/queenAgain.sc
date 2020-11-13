object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        (for (pos <- 0 until n;
              queens <- placeQueens(k - 1)
              if isSafe(pos, queens))
          yield pos :: queens).toSet
      }
    }
    def isSafe(pos: Int, queens: List[Int]): Boolean = {
      if (queens.isEmpty) true
      else
        !queens.contains(pos) && math.abs(pos - queens(0)) > 1
    }

    placeQueens(n)
  }
}

nqueens.queens(5) take (3)

//para n = 4 => Set[List(2,0,3,1),List(),...]