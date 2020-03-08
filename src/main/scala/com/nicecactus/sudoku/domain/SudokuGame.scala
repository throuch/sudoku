package com.nicecactus.sudoku.domain

class SudokuGame(val grid: Grid) {

  import Grid._

  def checkConstraints(row: Index, col: Index, digit: Digit): Boolean =
    checkLineConstraint(row, digit, col) && checkColumnConstraint(col, digit, row) && checkSubgridConstraint(row, col, digit)


  /**
   * row is optional, accelerate if provided
   *
   * @param col
   * @param digit
   * @param row
   * @return
   */
  def checkColumnConstraint(col: Index, digit: Digit, row: Index): Boolean =
    (grid.getDigit(row, col) == 0) && !getColumnDigits(col).contains(digit)

  /**
   * col is optional, accelerate if provided
   *
   * @param row
   * @param digit
   * @param col
   * @return
   */
  def checkLineConstraint(row: Index, digit: Digit, col: Index): Boolean =
    (grid.getDigit(row, col) == 0) && !getLineDigits(row).contains(digit)

  def checkSubgridConstraint(row: Index, col: Index, digit: Digit): Boolean =
    grid.getDigit(row, col) == 0 && !getSubgridDigits(row, col).contains(digit)


  def getSubgridDigits(row: Index, col: Index): Set[Digit] = {
    (for (
      i <- getSubgridIndices(row);
      j <- getSubgridIndices(col)
    ) yield grid.getDigit(i, j)).toSet - 0
  }

  def getLineDigits(row: Index): Set[Digit] = (for (j <- 1 to 9) yield grid.getDigit(row, j)).toSet - 0

  def getColumnDigits(col: Index): Set[Digit] = (for (i <- 1 to 9) yield grid.getDigit(i, col)).toSet - 0

}


