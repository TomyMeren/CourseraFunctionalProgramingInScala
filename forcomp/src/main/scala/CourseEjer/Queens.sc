import scala.math.abs

def isSafeOdersky(col: Int, queens: List[Int]): Boolean = {
  val row: Int = queens.length
  val queensWithRows = (row -1 to 0 by -1) zip queens
  println(queensWithRows)
  queensWithRows.forall{
    case (r,c) => col != c && abs(col - c)!= row -r
  }
}
isSafeOdersky(2,List(0,3,1))


def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    if (queens.isEmpty) true
    else !queens.contains(col) && abs(col - queens.head) > 1
  }

  def placeQueen(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queen <- placeQueen(k - 1)
        col <- 0 until n
        if isSafe(col, queen)
      } yield col :: queen
  }
  placeQueen(n)
}

queens(4)