package com.thornconsulting.sudoku.domain

import com.thornconsulting.sudoku.domain.Grid.getSubgridIndicesRolling
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SudokuSolverTest extends AnyFlatSpec with Matchers {


  it should "solve" in {
    val gridFixture = Array(
      Array(0, 0, 5, 3, 0, 0, 0, 0, 0),
      Array(8, 0, 0, 0, 0, 0, 0, 2, 0),
      Array(0, 7, 0, 0, 1, 0, 5, 0, 0),
      Array(4, 0, 0, 0, 0, 5, 3, 0, 0),
      Array(0, 1, 0, 0, 7, 0, 0, 0, 6),
      Array(0, 0, 3, 2, 0, 0, 0, 8, 0),
      Array(0, 6, 0, 5, 0, 0, 0, 0, 9),
      Array(0, 0, 4, 0, 0, 0, 0, 3, 0),
      Array(0, 0, 0, 0, 0, 9, 7, 0, 0)
    )

    new SudokuSolver(new SudokuGame(new Grid(gridFixture))).solve()

    gridFixture shouldEqual Array(
      Array(1, 4, 5, 3, 2, 7, 6, 9, 8),
      Array(8, 3, 9, 6, 5, 4, 1, 2, 7),
      Array(6, 7, 2, 9, 1, 8, 5, 4, 3),
      Array(4, 9, 6, 1, 8, 5, 3, 7, 2),
      Array(2, 1, 8, 4, 7, 3, 9, 5, 6),
      Array(7, 5, 3, 2, 9, 6, 4, 8, 1),
      Array(3, 6, 7, 5, 4, 2, 8, 1, 9),
      Array(9, 8, 4, 7, 6, 1, 2, 3, 5),
      Array(5, 2, 1, 8, 3, 9, 7, 6, 4))


  }

  it should "possibleValues" in {
    def nextInGrid(startRow: Index, startCol: Index): Iterator[(Index, Index)] =
      (for {
        row <- getSubgridIndicesRolling(startRow)
        col <- getSubgridIndicesRolling(startCol)
      } yield (row, col)).iterator

    nextInGrid(4, 4).foreach(x => println(s" x = ${x._1}  y = ${x._2}"))

    //println(new SudokuSolver(new SudokuGame(new Grid(gridFixture))).possibleValues(2, 1).mkString(" "))
  }


  it should "solve rapidly hardest Sudoku" in {
    val gridFixture2 =
      Array(
        Array(8, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 3, 6, 0, 0, 0, 0, 0),
        Array(0, 7, 0, 0, 9, 0, 2, 0, 0),
        Array(0, 5, 0, 0, 0, 7, 0, 0, 0),
        Array(0, 0, 0, 0, 4, 5, 7, 0, 0),
        Array(0, 0, 0, 1, 0, 0, 0, 3, 0),
        Array(0, 0, 1, 0, 0, 0, 0, 6, 8),
        Array(0, 0, 8, 5, 0, 0, 0, 1, 0),
        Array(0, 9, 0, 0, 0, 0, 4, 0, 0))


    new SudokuSolver(new SudokuGame(new Grid(gridFixture2))).solve()

    gridFixture2 shouldEqual Array(
      Array(8, 1, 2, 7, 5, 3, 6, 4, 9),
      Array(9, 4, 3, 6, 8, 2, 1, 7, 5),
      Array(6, 7, 5, 4, 9, 1, 2, 8, 3),
      Array(1, 5, 4, 2, 3, 7, 8, 9, 6),
      Array(3, 6, 9, 8, 4, 5, 7, 2, 1),
      Array(2, 8, 7, 1, 6, 9, 5, 3, 4),
      Array(5, 2, 1, 9, 7, 4, 3, 6, 8),
      Array(4, 3, 8, 5, 2, 6, 9, 1, 7),
      Array(7, 9, 6, 3, 1, 8, 4, 5, 2))


  }
}
