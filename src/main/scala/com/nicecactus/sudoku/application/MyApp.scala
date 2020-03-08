package com.nicecactus.sudoku.application

object MyApp extends App {

  val array = Array.ofDim[Int](9, 9)

  for (i <- 0 until 9) {

    array(i) = readLine.toCharArray.map(_.asDigit)

  }
  Console.err.println(array(0)(0).toString)
  Console.err.println(array(7)(8).toString)


}
