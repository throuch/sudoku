package com.thornconsulting.sudoku.domain

class SudokuGame(val grid: Grid) {

  import Grid._

  def checkConstraints(row: Index, col: Index, digit: Digit): Boolean =
    checkLineConstraint(row, digit, Some(col)) &&
      checkColumnConstraint(col, digit, Some(row)) &&
      checkSubgridConstraint(row, col, digit)


  /**
   * row is optional, accelerate if provided
   *
   * @param col
   * @param digit
   * @param row
   * @return
   */
  def checkColumnConstraint(col: Index, digit: Digit, row: Option[Index] = None): Boolean =
    row.fold(true)(grid.getDigit(_, col) == 0) && !getColumnDigits(col).contains(digit)

  /**
   * col is optional, accelerate if provided
   *
   * @param row
   * @param digit
   * @param col
   * @return
   */
  def checkLineConstraint(row: Index, digit: Digit, col: Option[Index] = None): Boolean =
    col.fold(true)(grid.getDigit(row, _) == 0) && !getLineDigits(row).contains(digit)

  def checkSubgridConstraint(row: Index, col: Index, digit: Digit): Boolean =
    grid.getDigit(row, col) == 0 && !getSubgridDigits(row, col).contains(digit)


  def getSubgridDigits(row: Index, col: Index): Set[Digit] =
    (for (
      i <- getSubgridIndices(row);
      j <- getSubgridIndices(col)
    ) yield grid.getDigit(i, j)).toSet - 0

  def getLineDigits(row: Index): Set[Digit] = (1 to 9).map(grid.getDigit(row, _)).toSet - 0

  def getColumnDigits(col: Index): Set[Digit] = (1 to 9).map(grid.getDigit(_, col)).toSet - 0

}


