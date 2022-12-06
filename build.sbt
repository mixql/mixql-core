val Scala3 = "3.2.1"
val Scala213 = "2.13.8"
val Scala212 = "2.12.17"
val ScalaVersions = Seq(Scala212, Scala213, Scala3)

ThisBuild / scalaVersion := Scala212

Antlr4 / antlr4Version := "4.8-1"
Antlr4 / antlr4GenListener := false // default: true
Antlr4 / antlr4GenVisitor := true // default: true

inThisBuild(
  List(
    organization := "org.mixql",
    version := "0.1.0-SNAPSHOT",
    homepage := Some(url("https://github.com/mixql/mixql-protobuf.git")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "LavrVV",
        "mixql team",
        "lavr3x@rambler.ru",
        sbt.url("http://mixql.org/")
      ),
      Developer(
        "wiikviz ",
        "mixql team",
        "kviz@outlook.com",
        sbt.url("http://mixql.org/")
      ),
      Developer(
        "mihan1235",
        "mixql team",
        "mihan1235@yandex.ru",
        sbt.url("http://mixql.org/")
      )
    )
  )
)

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "mixql-core",
    crossScalaVersions := ScalaVersions,
    libraryDependencies ++= Seq(
      "org.antlr"      % "antlr4-runtime" % "4.8-1",
      "org.scala-lang" % "scala-reflect" % {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => scalaVersion.value
          case Some((2, 12)) => scalaVersion.value
          case Some((3, _))  => "2.13.8"
        }
      },
      "org.apache.logging.log4j" % "log4j-api"  % "2.19.0",
      "org.apache.logging.log4j" % "log4j-core" % "2.19.0",
      "org.ow2.asm"              % "asm"        % "9.3",
      "org.ow2.asm"              % "asm-tree"   % "9.3",
      "org.scalatest"           %% "scalatest" % {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => "3.1.1"
          case Some((2, 12)) => "3.1.1"
          case Some((3, _))  => "3.2.14"
        }
      } % Test
    ),
    scalacOptions := {
      val stdOptions = Seq("-feature", "-deprecation")
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => stdOptions
        case Some((2, 12)) => stdOptions
        case Some((3, _)) =>
          stdOptions ++
            Seq(
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
      }
    }
  )
