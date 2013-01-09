name := "Jeeves Library"

version := "1.0"

scalaVersion := "2.10.0"

scalaHome := Some(file("/home/jeanyang/Tools/scala-2.10.0"))

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-language:dynamics", "-language:implicitConversions", "-language:reflectiveCalls", "-feature")
