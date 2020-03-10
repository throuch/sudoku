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


  def validateDimensions(): Boolean =
    numbers.forall(_.length == columnCount) && numbers.length == rowCount

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

  def isEmpty(row: Index, col: Index): Boolean = (numbers(row - 1)(col - 1) == 0)

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

  /**
   * examples:
   * 4 -> (4, 5, 6)
   * 8 -> (7, 8, 9)
   * 3 -> (1, 2, 3)
   */
  def getSubgridIndices(idx: Index): List[Index] = {
    val base: Index = idx - ((idx - 1) % 3)
    List(base, base + 1, base + 2)
  }

  /**
   * examples:
   * 4 -> (4, 5, 6)
   * 8 -> (8, 9, 7)
   * 3 -> (3, 1, 2)
   *
   * @param idx
   * @return
   */
  def getSubgridIndicesRolling(idx: Index): List[Index] = {
    val offset = ((idx - 1) % 3)
    val base = idx - offset
    List(idx, (offset + 1) % 3 + base, (offset + 2) % 3 + base)
  }

}