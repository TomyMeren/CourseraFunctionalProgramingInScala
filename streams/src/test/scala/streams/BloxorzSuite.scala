package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {

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
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
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

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `terrainFunction`: Unit =
    new Level1 {
      assertEquals(false,
        terrainFunction(Vector(Vector('S', 'T'), Vector('o', '-'), Vector('o', 'o')))(Pos(1, 1)))
    }

  @Test def `findchar`: Unit =
    new Level1 {
      println(startPos)
      println(goal)
    }

  @Test def `isLegal  `: Unit =
    new Level1 {
      assertEquals(Block(Pos(0, 1), Pos(0, 2)).isLegal, true)
    }

  @Test def `starblock  `: Unit =
    new Level1 {
      assertEquals(startBlock, Block(Pos(1, 1), Pos(1, 1)))
    }

  @Test def `neigbourg and legalNeighbourg`: Unit =
    new Level1 {
      println(startBlock.legalNeighbors)
    }

  @Test def `done `: Unit =
    new Level1 {
      println(goal)
      println(done(Block(goal, Pos(4, 8))))
    }

  @Test def `neighborsWithHistory  `: Unit =
    new Level1 {
      println(neighborsWithHistory(startBlock, List(Right, Up)).take(2).toList)
    }

  @Test def `newNeighborsOnly  `: Unit =
    new Level1 {
      assertEquals(newNeighborsOnly(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).to(LazyList),
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))).toList,
        List((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }


  //TODO:Optimizar
  @Test def `from  `: Unit =
    new Level1 {
      println(from(LazyList(
        (Block(Pos(1, 2), Pos(1, 3)), List(Left, Up))),
        Set(Block(Pos(1, 2), Pos(1, 3)))).take(30).toList.map(x=> x._1).distinct.length
        /*,LazyList(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)))*/
      )
    }

  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      println(solve(solution))
      //assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
