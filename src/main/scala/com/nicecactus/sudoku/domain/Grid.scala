package com.nicecactus.sudoku.domain


/**
 * This is a Sudoku grid of dimension 9x9
 *
 * the numbers in the grid are in the range [0,9]
 *
 * indices are in the range [1,9]
 *
 * @param numbers
 */
class Grid(val numbers: Array[Array[Digit]]) {

  val columnCount: Int = 9
  val rowCount: Int = 9

  assert(validateDimensions())
  assert(validateContent())


  def validateDimensions(): Boolean = {
    numbers.forall(_.length == columnCount) && numbers.length == rowCount
  }

  /**
   * test
   *
   * @param row
   * @param col
   * @param number
   */
  def setDigit(row: Index, col: Index, number: Digit): Unit = {
    //assert( row  ...)
    numbers(row - 1)(col - 1) = number
  }

  def getDigit(row: Index, col: Index): Digit =
    numbers(row - 1)(col - 1)

  def validateContent(): Boolean = {

    for (i <- 0 until rowCount; j <- 0 until columnCount) {
      if (!(numbers(i)(j) >= 0 && numbers(i)(j) <= 9))
        return false
    }


    true
  }


  def display() = {
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        // Accessing the values
        print(" " + numbers(i)(j))
      }
      println()
    }

  }
}


object Grid {

  def getSubgridIndices(idx: Index): List[Index] = {

    val base: Index = idx - ((idx - 1) % 3)

    List(base, base + 1, base + 2)
  }


}