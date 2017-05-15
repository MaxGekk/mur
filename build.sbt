name := "mur"

version := "1.0"

scalaVersion := "2.12.2"

val scalaTestV = "3.0.1"
val parserV = "1.0.6"
val swingV = "2.0.0"
val akkaV = "2.5.1"

coverageEnabled := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestV % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % parserV,
  "org.scala-lang.modules" %% "scala-swing" % swingV,
  "com.typesafe.akka" %% "akka-actor" % akkaV
)

mainClass in assembly := Some("mur.Editor")
assemblyJarName in assembly := "mur-editor.jar"
