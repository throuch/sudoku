package com.nicecactus.sudoku.application.injection

import com.nicecactus.sudoku.domain.{Grid, SudokuGame}
import com.nicecactus.sudoku.interfaces.Sudoku

object Module {

  class DefaultSudokuGame extends SudokuGame(new Grid(Array.ofDim[Int](9, 9))) with Sudoku

}
