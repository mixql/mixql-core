scalaVersion := Scala212

Antlr4 / antlr4Version := "4.8-1"
Antlr4 / antlr4GenListener := false // default: true
Antlr4 / antlr4GenVisitor := true // default: true
Antlr4 / antlr4PackageName := Some("org.mixql.core.generated")
Antlr4 / antlr4FolderToClean := (Antlr4 / javaSource).value / "org" / "mixql" / "core" / "generated"

lazy val mixQLCore = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials"),
    organization := "org.mixql",
    name := "mixql-core",
    version := "0.5.1",
    crossScalaVersions := ScalaVersions,
    organizationName := "MixQL",
    organizationHomepage := Some(url("https://mixql.org/")),
    homepage := Some(url("https://github.com/mixql/mixql-core")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/mixql/mixql-core"),
        "scm:git@github.com:mixql/mixql-core.git"
      )
    ),
    developers := List(
      Developer(
        "LavrVV",
        "MixQL team",
        "lavr3x@rambler.ru",
        url("https://github.com/LavrVV")
      ),
      Developer(
        "wiikviz",
        "Kostya Kviz",
        "kviz@outlook.com",
        url("https://github.com/wiikviz")
      ),
      Developer(
        "mihan1235",
        "MixQL team",
        "mihan1235@yandex.ru",
        url("http://mixql.org/")
      ),
      Developer(
        "ntlegion",
        "MixQL team",
        "ntlegion@outlook.com",
        url("https://github.com/ntlegion")
      )
    ),
    description := "Mixed query language.",
    licenses := List(
      "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
    ),
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://s01.oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "content/repositories/releases/")
    },
    libraryDependencies ++= Seq(
      "org.antlr"      % "antlr4-runtime" % "4.8-1",
      "com.typesafe"   % "config"         % "1.4.2",
      "org.scala-lang" % "scala-reflect" % {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => scalaVersion.value
          case Some((2, 12)) => scalaVersion.value
          case Some((3, _))  => "2.13.8"
        }
      },
      "org.apache.logging.log4j" % "log4j-api"  % "2.19.0",
      "org.apache.logging.log4j" % "log4j-core" % "2.19.0",
      // "org.ow2.asm"              % "asm"        % "9.3",
      // "org.ow2.asm"              % "asm-tree"   % "9.3",
      "org.scalatest" %% "scalatest" % {
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
val Scala3 = "3.1.3"
val Scala213 = "2.13.8"
val Scala212 = "2.12.17"
val ScalaVersions = Seq(Scala212, Scala213, Scala3)
