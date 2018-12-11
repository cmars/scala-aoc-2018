name := "scala-aoc-2018"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.3.4" % "test")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

scalacOptions in Test ++= Seq("-Yrangepos")