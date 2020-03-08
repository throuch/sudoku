package com.nicecactus.sudoku.domain

class SudokuSolver(game: SudokuGame) {

  import Grid._


  val digitList = (1 to 9).toSet


  var startingRow: Int = 1
  var startingCol: Int = 1
  var flag = false

  var next: (Index, Index) => (Index, Index) = nextCol

  def solve(): Unit = {

    val (srow, scol) = findBestStartBox

    Console.err.println(s"Debug: starting from ($srow, $scol)")
    startingRow = getSubgridIndices(srow)(0)
    startingCol = getSubgridIndices(scol)(0)

    if (game.getLineDigits(startingRow).size < game.getColumnDigits(startingCol).size)
      next = nextRow

    val res = tryCombo(startingRow, startingCol)
    //println("")
    game.grid.display()
    //println(s"solved = $res")
  }

  def findBestSequence(): IndexedSeq[(Index, Index)] = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield
      (r, c)).sortBy(x => -(game.getSubgridDigits(x._1, x._2).size + game.getLineDigits(x._1).size + game.getColumnDigits(x._2).size))
  }


  def findBestStartBox(): (Index, Index) = {
    (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
      foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
        val max = game.getSubgridDigits(e._1, e._2).size + game.getLineDigits(e._1).size + game.getColumnDigits(e._2).size
        if (max > acc._1)
          (max, e)
        else
          acc
      })._2

  }


  def possibleValues(row: Index, col: Index): Set[Digit] =
    digitList -- game.getSubgridDigits(row, col) -- game.getLineDigits(row) -- game.getColumnDigits(col)

  private def nextCol(row: Index, col: Index): (Index, Index) = {
    //assert(row >= 1 && row <= 9)
    //assert(col >= 1 && col <= 9)
    val next_col = (col % 9) + 1

    (if (next_col == startingCol) (row % 9) + 1 else row, next_col)
  }

  private def nextRow(row: Index, col: Index): (Index, Index) = {
    //assert(row >= 1 && row <= 9)
    //assert(col >= 1 && col <= 9)
    val next_row = (row % 9) + 1
    (next_row, if (next_row == startingRow) (col % 9) + 1 else col)
  }

  private def tryCombo(row: Index, col: Index): Boolean = {
    //println(s" row: $row col: $col")
    if (row == startingRow && col == startingCol) {
      if (flag)
        return true
      else
        flag = true
    }

    val freeBox = (game.grid.getDigit(row, col) == 0)
    val (next_row, next_col) = next(row, col)

    if (freeBox) {

      for (v <- possibleValues(row, col) if game.checkConstraints(row, col, v)) {
        game.grid.setDigit(row, col, v)

        if (tryCombo(next_row, next_col))
          return true
        game.grid.setDigit(row, col, 0)
      }

      false
    }
    else
      tryCombo(next_row, next_col)
  }
}

