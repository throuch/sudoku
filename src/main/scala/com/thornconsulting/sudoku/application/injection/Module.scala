package com.thornconsulting.sudoku.application.injection

import com.thornconsulting.sudoku.domain.{Grid, SudokuGame}
import com.thornconsulting.sudoku.interfaces.Sudoku

object Module {

  class DefaultSudokuGame extends SudokuGame(new Grid(Array.ofDim[Int](9, 9))) with Sudoku

}
