
def countChange(money: Int, coins: List[Int]): Int = {
  //println(money: Int, coins: List[Int])
  if (coins.isEmpty || money < 0) 0
  else if (money == 0) 1
  else countChange(money - coins.head, coins) +
    countChange(money - coins.head, coins.tail)
}

countChange(6, List(1, 2, 3)) // 3

countChange(2, List(1)) // 1

countChange(300,List(5,10,20,50,100,200,500)) // 1022