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

  @Test def `test isStanding`: Unit =
    new Level1 {
      assert(startBlock.isStanding)
      val rightBlock = startBlock.right
      assert(!rightBlock.isStanding)
      val downBlock = rightBlock.down
      assert(!downBlock.isStanding)
      val leftBlock = downBlock.left
      assert(leftBlock.isStanding)
    }

  @Test def `test legalNeighbours`: Unit =
    new Level1 {
      val neighbours = startBlock.neighbors
      assertEquals(4, neighbours.size)

      val legalNeighbours = startBlock.legalNeighbors
      assertEquals(2, legalNeighbours.size)

      val downBlock = startBlock.down
      val downNeighbours = downBlock.neighbors
      assertEquals(4, downNeighbours.size)

      val downLegalNeighbours = downBlock.legalNeighbors
      assertEquals(2, downLegalNeighbours.size)
    }

  @Test def `test neighboursWithHistory`: Unit =
    new Level1 {
      val firstNeighbours = neighborsWithHistory(startBlock, List())
      assertEquals(firstNeighbours.size, 2)

      val (downBlock, downMoves) = firstNeighbours find { case (block, moves) => moves.head == Down } getOrElse((null, null))
      val nextNeighbours = neighborsWithHistory(downBlock, downMoves)

      assertEquals(nextNeighbours.length, 2)
      val (rightBlock, rightMoves) = nextNeighbours find { case (block, moves) => moves.head == Right} getOrElse((null, null))
      assertEquals(rightMoves.tail.head, Down)
      assertEquals(rightMoves.length, 2)
    }

  @Test def `test neighboursWithHistoryTwo`: Unit =
    new Level1 {
      val neighbours = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      assertEquals(neighbours.toSet, Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }

  @Test def `test newNeighborsOnly`: Unit =
    new Level1 {
      val result = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )

      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).to(LazyList);

      assertEquals(result, expected)
    }

  @Test def `test done`: Unit =
    new Level1 {
      assertEquals(done(startBlock), false)
      val endBlock = solve(optsolution)
      assertEquals(done(endBlock), true)
    }

  @Test def `test from`: Unit =
    new Level1 {
      val neighbours = neighborsWithHistory(startBlock, List())
      val initial = newNeighborsOnly(neighbours, Set())
      val res = from(initial, Set())
      println(res.toList)
    }

  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }


  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
