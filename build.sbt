
name := "Sudoku"

version := "0.1"

scalaVersion := "2.12.7"

lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"


lazy val root = (project in file("."))
  .settings(
    name := "Sudoku",
    libraryDependencies += scalaTest % Test
  )