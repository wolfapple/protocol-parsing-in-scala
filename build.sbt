name := "protocol-test"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scodec" %% "scodec-bits" % "1.1.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
)