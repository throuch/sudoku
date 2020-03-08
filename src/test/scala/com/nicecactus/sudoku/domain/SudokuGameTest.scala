package com.nicecactus.sudoku.domain


class SudokuGameTest extends org.scalatest.FlatSpec {

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

  it should "validateDimensions" in {

    //fail("test not implemented")

  }

  it should "validateDigit" in {
    val game = new SudokuGame(new Grid(gridFixture))

    // check central grid

    assert(game.checkConstraints(1, 1, 9))
    assert(game.checkConstraints(1, 1, 6))

    assert(game.checkConstraints(4, 4, 1))
    assert(game.checkConstraints(4, 4, 8))
    assert(game.checkConstraints(4, 4, 6))
    assert(game.checkConstraints(4, 4, 9))

    assert(!game.checkConstraints(4, 4, 7))
    assert(!game.checkConstraints(4, 4, 4))
    assert(!game.checkConstraints(5, 4, 1))

  }

  it should "checkColumnConstraint" in {
    val game = new SudokuGame(new Grid(gridFixture))

    assert(!game.checkColumnConstraint(3, 5))
    assert(!game.checkColumnConstraint(3, 3))
    assert(!game.checkColumnConstraint(3, 4))
    assert(game.checkColumnConstraint(3, 1))
    assert(game.checkColumnConstraint(3, 2))
  }

  it should "checkLineConstraint" in {
    val game = new SudokuGame(new Grid(gridFixture))

    assert(!game.checkLineConstraint(3, 7))
    assert(!game.checkLineConstraint(3, 1))
    assert(!game.checkLineConstraint(3, 5))

    assert(game.checkLineConstraint(3, 2))
    assert(game.checkLineConstraint(3, 3))
    assert(game.checkLineConstraint(3, 4))
    assert(game.checkLineConstraint(3, 6))
  }

  it should "checkSubgridConstraint" in {
    val game = new SudokuGame(new Grid(gridFixture))


    assert(game.checkSubgridConstraint(4, 4, 1))
    assert(game.checkSubgridConstraint(4, 4, 3))

    assert(!game.checkSubgridConstraint(4, 4, 2))
    assert(!game.checkSubgridConstraint(6, 4, 1))
  }

  it should "getSubgridDigits" in {

    val game = new SudokuGame(new Grid(gridFixture))

    assert(game.grid.getDigit(4, 6) == 5)
    assert(game.grid.getDigit(5, 6) == 0)
    assert(game.getSubgridDigits(5, 5) == Set(2, 5, 7))
    assert(game.getSubgridDigits(5, 6) == Set(2, 5, 7))
    assert(game.getSubgridDigits(5, 4) == Set(2, 5, 7))
  }


}
