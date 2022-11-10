ThisBuild / scalaVersion := "2.13.8"

Antlr4 / antlr4Version := "4.8-1"
Antlr4 / antlr4GenListener := false // default: true
Antlr4 / antlr4GenVisitor := true // default: true

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    organization := "org.grenki",
    name := "gsql",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.antlr"                % "antlr4-runtime" % "4.8-1",
      "org.scala-lang"           % "scala-reflect"  % "2.13.8",
      "org.apache.logging.log4j" % "log4j-api"      % "2.19.0",
      "org.apache.logging.log4j" % "log4j-core"     % "2.19.0",
      "org.scalatest"           %% "scalatest"      % "3.1.1" % Test
    ),
    scalacOptions ++= Seq("-feature", "-deprecation")
  )
