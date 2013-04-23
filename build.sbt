import AssemblyKeys._

name := "Jeeves Library"

version := "1.0"

scalaVersion := "2.10.0"

crossScalaVersions := Seq("2.9.0-1", "2.9.1", "2.10.0")

scalaHome := Some(file(sys.env("SCALA_HOME")))

libraryDependencies <++= scalaVersion(v => v match {
  case "2.9.0-1" =>
    Seq(
      "org.scalatest" %% "scalatest" % "1.8" % "test"
    , "org.specs2" %% "specs2" % "1.8.2" % "test"
    , "org.squeryl" %% "squeryl" % "0.9.5-2")
  case "2.9.1" =>
    Seq(
      "org.scalatest" %% "scalatest" % "1.8" % "test"
    , "org.specs2" %% "specs2" % "1.6.9" % "test"
    , "org.squeryl" %% "squeryl" % "0.9.5-2")
  case "2.10.0" =>
    Seq(
      "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    , "org.specs2" %% "specs2" % "1.14" % "test"
    , "org.squeryl" %% "squeryl" % "0.9.5-6")
})

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.168",
  "mysql" % "mysql-connector-java" % "5.1.19")

scalacOptions <++= scalaVersion map (v => v match {
  case "2.9.0-1" => Seq("-deprecation", "-unchecked", "-Xexperimental")
  case "2.9.1" => Seq("-deprecation", "-unchecked", "-Xexperimental")
  case "2.10.0" => Seq("-deprecation", "-unchecked", "-language:dynamics", "-language:implicitConversions", "-language:reflectiveCalls", "-feature")
})

fork in run := true

javaOptions ++= Seq(
  "-agentlib:hprof=cpu=times,file=/home/jeanyang/Documents/jeeveslib/hprof.txt"
)

assemblySettings

jarName in assembly := "runjeeves.jar"

mainClass in assembly := Some("cap.jeeveslib.demo.battleship.BattleshipGame")
