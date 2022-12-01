ThisBuild / scalaVersion := "3.2.1"

Antlr4 / antlr4Version := "4.8-1"
Antlr4 / antlr4GenListener := false // default: true
Antlr4 / antlr4GenVisitor := true // default: true

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    organization := "org.mixql",
    name := "mixql-core",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.antlr"                % "antlr4-runtime" % "4.8-1",
      "org.scala-lang"           % "scala-reflect"  % "2.13.8",
      "org.apache.logging.log4j" % "log4j-api"      % "2.19.0",
      "org.apache.logging.log4j" % "log4j-core"     % "2.19.0",
      "org.ow2.asm"              % "asm"            % "9.3",
      "org.ow2.asm"              % "asm-tree"       % "9.3",
      "org.scalatest"           %% "scalatest"      % "3.2.14" % Test
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      ///////////////////////////////////////////////////////////////////////////////////
      // https://docs.scala-lang.org/scala3/guides/migration/tooling-syntax-rewriting.html
//      "-new-syntax", "-rewrite",
//      "-indent", "-rewrite",
      ///////////////////////////////////////////////////////////////////////////////////
//      "-source",
//      "3.0-migration",
      "-Xmax-inlines:139", // https://github.com/lampefl/dotty/issues/13044
      "-Xmax-inlined-trees:12000000" // https://github.com/lampefl/dotty/issues/13044
    )
  )
