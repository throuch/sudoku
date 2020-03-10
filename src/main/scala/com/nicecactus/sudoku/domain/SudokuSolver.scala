package com.nicecactus.sudoku.domain


class SudokuSolver(game: SudokuGame) {
  import Grid._

  type MyIterator = Iterator[(Index, Index)]

  val digitList = (1 to 9).toSet

  var emptyBoxes: Int = countEmptyBoxes()

  def solve(): Unit = {
    //SudokuSolver.displayOptima(game)
    tryCombo(Iterator.empty[(Index, Index)])
    game.grid.display()
  }

  def countEmptyBoxes(): Int =
    (for (r <- 1 to 9; c <- 1 to 9 if game.grid.isEmpty(r, c)) yield 1).size

  def possibleValues(row: Index, col: Index): Set[Digit] =
    digitList --
      game.getSubgridDigits(row, col) --
      game.getLineDigits(row) --
      game.getColumnDigits(col)

  private def nextCol(startRow: Index, startCol: Index): MyIterator =
    (for (
      col <- (-1 until 8);
      ccol = (startCol + col) % 9 + 1
      if game.grid.isEmpty(startRow, ccol)
    ) yield (startRow, ccol)).iterator

  private def nextRow(startRow: Index, startCol: Index): MyIterator =
    (for (
      row <- (-1 until 8);
      crow = (startRow + row) % 9 + 1
      if game.grid.isEmpty(crow, startCol)
    ) yield (crow, startCol)).iterator

  private def nextInGrid(startRow: Index, startCol: Index): MyIterator =
    (for {
      row <- getSubgridIndicesRolling(startRow)
      col <- getSubgridIndicesRolling(startCol)
      if game.grid.isEmpty(row, col)
    } yield (row, col)).iterator


  object Constraint extends Enumeration {
    type ConstraintType = Value
    val Row, Col, Grid = Value
  }

  import Constraint._

  def findBestStartBox(): (ConstraintType, (Index, Index)) = {
    (for (r <- 1 to 9; c <- 1 to 9 if game.grid.isEmpty(r, c)) yield (r, c)).
      foldLeft[((Int, Int), (ConstraintType, (Index, Index)))]((0, 0), (Row, (1, 1)))((acc, e) => {

        val e1 = game.getLineDigits(e._1)
        val e2 = game.getColumnDigits(e._2)
        val e3 = game.getSubgridDigits(e._1, e._2)
        val superMax = e1 ++ e2 ++ e3

        val max = List(
          (e1.size, superMax.size, (Row, e)),
          (e2.size, superMax.size, (Col, e)),
          (e3.size, superMax.size, (Constraint.Grid, e)),
          (acc._1._1, acc._1._2, acc._2)
        ).max[(Int, Int, _)](Ordering[(Int, Int)].on(x => (x._2, x._1)))

        ((max._1, max._2), max._3)
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
      case o if (o._1 == Constraint.Grid) => {
        //println(s" Grid Start:  (row:${o._2._1},col:${o._2._2}) ")
        nextInGrid _ tupled o._2
      }
    }


  private def tryCombo(inputIt: MyIterator): Boolean =
    (emptyBoxes == 0) || {

      val it = if (!inputIt.hasNext) findBestConstraintIterator() else inputIt
      val (row, col) = it.next()
      for (v <- possibleValues(row, col) if game.checkConstraints(row, col, v)) {
        game.grid.setDigit(row, col, v)
        emptyBoxes -= 1
        if (tryCombo(it))
          return true
        game.grid.setDigit(row, col, 0)
        emptyBoxes += 1
      }
      false
    }
}

object SudokuSolver {
  def displayOptima(game: SudokuGame): Unit = {
    Console.err.println(s"best subgrid : ${
      (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
        foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
          val max = game.getSubgridDigits(e._1, e._2).size

          if (max > acc._1)
            (max, e)
          else
            acc
        })
    }")
    Console.err.println(s"best column : ${
      (for (r <- 1 to 9; c <- 1 to 9) yield (r, c)).
        foldLeft[(Int, (Index, Index))]((0, (1, 1)))((acc, e) => {
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
}
