name := "symcal"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.3" % Test
  , "net.openhft" % "compiler" % "2.3.0"
)
