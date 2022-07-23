ThisBuild / scalaVersion := "2.13.8"

Antlr4 / antlr4GenListener := false // default: true
Antlr4 / antlr4GenVisitor := true // default: true

lazy val root = (project in file(".")).enablePlugins(Antlr4Plugin)
  .settings(
    organization := "org.grenki",
    name := "gsql",
    version := "0.1.0-SNAPSHOT"
  )
