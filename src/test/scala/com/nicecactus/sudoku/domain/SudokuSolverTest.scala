package com.nicecactus.sudoku.domain

import org.scalatest.FlatSpec

class SudokuSolverTest extends FlatSpec {

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

  it should "solve" in {
    //val game = new SudokuGame(new Grid(gridFixture))

    new SudokuSolver(new SudokuGame(new Grid(gridFixture))).solve()
    //new SudokuSolver(new SudokuGame(new Grid(gridFixture))).solve()
    //new SudokuSolver(new SudokuGame(new Grid(gridFixture))).solve()

    //assert(game.validateDigit(1, 1, 9))
    //assert(game.validateDigit(1, 1, 6))

  }

  it should "possibleValues" in {
    println(new SudokuSolver(new SudokuGame(new Grid(gridFixture))).possibleValues(2, 1).mkString(" "))
  }


  it should "solve rapidly" in {
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

  }
}
