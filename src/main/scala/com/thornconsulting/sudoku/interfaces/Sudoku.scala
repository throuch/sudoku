package com.thornconsulting.sudoku.interfaces

trait Sudoku {

  /**
   * Allows to validate a number in a sudoku grid
   *
   * @param line
   * @param column
   * @return
   */
  def checkConstraints(line: Int, column: Int, digit: Int): Boolean

}
