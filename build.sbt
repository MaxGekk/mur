name := "mur"

version := "1.0"

scalaVersion := "2.12.2"

val scalaTestV = "3.0.1"
val parserV = "1.0.6"

coverageEnabled := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestV % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % parserV

)        
