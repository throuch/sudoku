package com.nicecactus.sudoku.application


import scala.io.StdIn.readLine

object MyApp extends App {

  val array = Array.ofDim[Int](9, 9)

  for (i <- 0 until 9) {

    array(i) = readLine.toCharArray.map(_.asDigit)

  }


}
