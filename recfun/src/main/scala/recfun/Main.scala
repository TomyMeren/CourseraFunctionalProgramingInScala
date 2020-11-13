package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        //print(row + " / " + col + " ==> ")
        print(pascal(col, row) + " ")
      }
      println()
    }
  }
  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  /*  for (row <- 1 to 10) {
      for (col <- 1 to row)
        print(pascal(col, row) + " " )
      println()
    }*/

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def inner(chars: List[Char], cont: Int): Boolean = {
      if (chars.isEmpty) {
        if (cont == 0) true else false
      }
      else if (chars.head == ')') {
        if (cont > 0) inner(chars.tail, cont - 1)
        else false
      }
      else if (chars.head == '(') inner(chars.tail, cont + 1)
      else inner(chars.tail, cont)
    }
    inner(chars, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
