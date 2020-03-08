package com.nicecactus.sudoku.domain

class SudokuSolver(game: SudokuGame) {

  import Grid._


  val digitList = (1 to 9).toSet


  var nextIterator: Iterator[(Index, Index)] = Iterator.empty
  var emptyBoxes: Int = countEmptyBoxes()

  def solve(): Unit = {


    //    Console.err.println(s"Debug: starting from ($srow, $scol)")
    //    startingRow = getSubgridIndices(srow)(0)
    //    startingCol = getSubgridIndices(scol)(0)
    //    Console.err.println(s"Debug: starting from ($startingRow, $startingCol)")

    displayOptima()

    tryCombo()
    //println("")
    game.grid.display()
    //println(s"solved = $res")
  }


  def countEmptyBoxes(): Int = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield
      (r, c)).size
  }

  def displayOptima(): Unit = {

    println(s"best subgrid : ${
      (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
        foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
          val max = game.getSubgridDigits(e._1, e._2).size
          //        val max1 = game.getLineDigits(e._1).size
          //        val max2 = game.getColumnDigits(e._2).size
          //
          if (max > acc._1)
            (max, e)
          else
            acc
        })
    }")

    println(s"best column : ${
      (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
        foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
          //        val max1 = game.getLineDigits(e._1).size
          val max = game.getColumnDigits(e._2).size
          //
          if (max > acc._1)
            (max, e)
          else
            acc
        })
    }")

    println(s"best line : ${
      (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
        foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
          val max = game.getLineDigits(e._1).size
          //        val max2 = game.getColumnDigits(e._2).size
          //
          if (max > acc._1)
            (max, e)
          else
            acc
        })
    }")

  }

  def possibleValues(row: Index, col: Index): Set[Digit] =
    digitList -- game.getSubgridDigits(row, col) -- game.getLineDigits(row) -- game.getColumnDigits(col)

  private def nextCol(startRow: Index, startCol: Index): Iterator[(Index, Index)] = new Iterator[(Index, Index)] {
    //assert(row >= 1 && row <= 9)
    //assert(col >= 1 && col <= 9)
    var i = 9
    var col = startCol

    override def hasNext: Boolean = (i > 0)

    override def next(): (Index, Index) = {
      i -= 1
      val tcol = col
      col = (col % 9) + 1
      (startRow, tcol)
    }
  }

  private def nextRow(startRow: Index, startCol: Index): Iterator[(Index, Index)] = new Iterator[(Index, Index)] {
    //assert(row >= 1 && row <= 9)
    //assert(col >= 1 && col <= 9)

    var i = 9
    var row = startRow

    override def hasNext: Boolean = (i > 0)

    override def next(): (Index, Index) = {
      i -= 1
      val trow = row
      row = (row % 9) + 1
      (trow, startCol)
    }
  }

  private def nextInGrid(row: Index, col: Index): Iterator[(Index, Index)] = new Iterator[(Index, Index)] {
    //assert(row >= 1 && row <= 9)
    //assert(col >= 1 && col <= 9)

    var i = 9

    override def hasNext: Boolean = (i > 0)

    override def next(): (Index, Index) = {

      // TODO
      i -= 1
      ???
    }
  }

  def findFirstEmptyBox(): (Index, Index) = {
    //game.getColumnDigits(col)
    for (r <- 1 to 9; c <- 1 to 9) {

      if (game.grid.getDigit(r, c) == 0)
        return (r, c)
    }
    (-1, -1)
  }

  def findBestStartBoxByRow(): (Int, (Index, Index)) = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield (r, c)).
      foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
        //        val sgrid = game.getSubgridDigits(e._1, e._2).size
        //        val max1 = game.getLineDigits(e._1).size + sgrid
        val max = game.getColumnDigits(e._2).size
        //        val max = Math.max(max1, max2)
        if (max > acc._1)
          (max, e)
        else
          acc
      })
  }

  def findBestStartBoxByCol(): (Int, (Index, Index)) = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield (r, c)).
      foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
        //        val sgrid = game.getSubgridDigits(e._1, e._2).size
        val max = game.getLineDigits(e._1).size

        if (max > acc._1)
          (max, e)
        else
          acc
      })
  }

  def findBestStartBoxBySubgrid(): (Index, Index) = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield (r, c)).
      foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
        val max = game.getSubgridDigits(e._1, e._2).size

        if (max > acc._1)
          (max, e)
        else
          acc
      })._2
  }


  def findBestConstraintIterator(): Iterator[(Index, Index)] = {
    //val (startRow, startCol) = findFirstEmptyBox()

    //nextRow(startRow, startCol)
    val rowOptim = findBestStartBoxByRow()
    val colOptim = findBestStartBoxByCol()
    if (rowOptim._1 > colOptim._1)
      nextRow _ tupled rowOptim._2
    else
      nextCol _ tupled colOptim._2

  }

  private def next(): (Index, Index) = {
    if (!nextIterator.hasNext) {
      nextIterator = findBestConstraintIterator()
    }
    nextIterator.next()
  }

  private def tryCombo(): Boolean = {
    //println(s" row: $row col: $col")
    if (emptyBoxes == 0)
      return true

    val (row, col) = next()

    val freeBox = (game.grid.getDigit(row, col) == 0)

    if (freeBox) {

      for (v <- possibleValues(row, col) if game.checkConstraints(row, col, v)) {
        game.grid.setDigit(row, col, v)
        emptyBoxes -= 1
        if (tryCombo())
          return true
        game.grid.setDigit(row, col, 0)
        emptyBoxes += 1
      }

      false
    }
    else {

      tryCombo()
    }
  }
}

