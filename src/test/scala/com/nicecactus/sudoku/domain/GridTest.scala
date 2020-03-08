package com.nicecactus.sudoku.domain

import org.scalatest.FlatSpec

class GridTest extends FlatSpec {
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

  behavior of "GridTest"

  it should "getDigit" in {
    cancel("test not implemented")
  }

  it should "setDigit" in {
    cancel("test not implemented")
  }

  it should "validateContent" in {
    cancel("test not implemented")
  }

  it should "validateDimensions" in {
    assert(new Grid(gridFixture).validateDimensions())
  }

  it should "rowCount" in {
    cancel("test not implemented")
  }

  it should "colCount" in {
    cancel("test not implemented")
  }

  it should "getSubgridIndices" in {
    assert(Grid.getSubgridIndices(4) == List(4, 5, 6))
    assert(Grid.getSubgridIndices(5) == List(4, 5, 6))
    assert(Grid.getSubgridIndices(6) == List(4, 5, 6))

  }
}
