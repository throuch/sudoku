
name := "Sudoku"

version := "0.1"

scalaVersion := "2.13.1"

lazy val scalaTest = Seq(
  "org.scalatest" %% "scalatest" % "3.1.0"% Test,
  "org.scalactic" %% "scalactic" % "3.1.0" % Test)


lazy val root = (project in file("."))
  .settings(
    name := "Sudoku",
    libraryDependencies ++= scalaTest
  )