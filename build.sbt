name := "symcal"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.3" % Test
//  , "org.scala-lang" % "scala-reflect" % scalaVersion.value
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
)
