package com.nicecactus.sudoku.domain


//import shuffle
class SudokuSolver(game: SudokuGame) {

  import Grid._

  trait MyIterator extends Iterator[(Index, Index)] {
    def rewind: Unit
  }


  val digitList = (1 to 9).toSet


  var nextIterator: MyIterator = new MyIterator {
    override def hasNext = false

    override def next(): (Index, Index) = ???

    override def rewind: Unit = ???
  }

  var emptyBoxes: Int = countEmptyBoxes()

  def solve(): Unit = {


    //    Console.err.println(s"Debug: starting from ($srow, $scol)")
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

    Console.err.println(s"best subgrid : ${
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

    Console.err.println(s"best column : ${
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

    Console.err.println(s"best line : ${
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
    digitList --
      game.getSubgridDigits(row, col) --
      game.getLineDigits(row) --
      game.getColumnDigits(col)

  private def nextCol(startRow: Index, startCol: Index): MyIterator = new MyIterator {
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

    override def rewind: Unit = {
      col = ((col + 7) % 9) + 1
      i += 1
    }

  }

  private def nextRow(startRow: Index, startCol: Index): MyIterator = new MyIterator {
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

    override def rewind: Unit = {
      row = ((row + 7) % 9) + 1
      i += 1
    }
  }

  private def nextInGrid(startRow: Index, startCol: Index): MyIterator =
    new MyIterator {

      var currentIdx = 0
      val values = (for {
        row <- getSubgridIndices(startRow)
        col <- getSubgridIndices(startCol)
        if (game.grid.getDigit(row, col) == 0)
      } yield (row, col))


      override def hasNext = (currentIdx < values.length)

      override def next(): (Index, Index) = {
        val res = values(currentIdx)
        currentIdx += 1
        res
      }

      override def rewind: Unit = {
        currentIdx -= 1
      }

    }


  object Constraint extends Enumeration {
    type ConstraintType = Value
    val Row, Col, Grid = Value
  }

  import Constraint._

  def findBestStartBox(): (ConstraintType, (Index, Index)) = {
    (for (r <- 1 to 9; c <- 1 to 9 if (game.grid.getDigit(r, c) == 0)) yield (r, c)).
      foldLeft[((Int, Int), (ConstraintType, (Index, Index)))]((0, 0), (Row, (1, 1)))((acc, e) => {

        val e1 = game.getLineDigits(e._1).size
        val e2 = game.getColumnDigits(e._2).size
        val e3 = game.getSubgridDigits(e._1, e._2).size
        val superMax = e1 + e2 + e3

        val max = List(
          (e1, superMax, Row),
          (e2, superMax, Col),
          (e3, superMax, Grid)).maxBy(_._1)

        //Ordering[(Int, String)].on(x => (x._1 , "toot"))
        if ((max._1 > acc._1._1) || ((max._1 == acc._1._1) && (max._2 > acc._1._2))) {
          ((max._1, max._2), (max._3, e))
        } else
          acc
      })._2
  }


  def findBestConstraintIterator(): MyIterator =
    findBestStartBox() match {
      case o if (o._1 == Row) => {
        //println(s" Col Start:  (row:${o._2._1},col:${o._2._2}) ")
        nextCol _ tupled o._2
      }
      case o if (o._1 == Col) => {
        //println(s" Row Start:  (row:${o._2._1},col:${o._2._2}) ")
        nextRow _ tupled o._2
      }
      case o if (o._1 == Grid) => {
        //println(s" Grid Start:  (row:${o._2._1},col:${o._2._2}) ")
        nextInGrid _ tupled o._2
      }
    }

  private def nextIteratorFunc(): MyIterator = {
    if (!nextIterator.hasNext) {
      nextIterator = findBestConstraintIterator()
    }
    nextIterator
  }

  private def tryCombo(): Boolean = {
    //println(s" row: $row col: $col")
    if (emptyBoxes == 0)
      return true

    val it = nextIteratorFunc
    val (row, col) = it.next()

    val freeBox = (game.grid.getDigit(row, col) == 0)
    //println("loop")
    if (freeBox) {

      for (v <- possibleValues(row, col) if game.checkConstraints(row, col, v)) {
        game.grid.setDigit(row, col, v)
        emptyBoxes -= 1
        if (tryCombo())
          return true
        it.rewind
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

