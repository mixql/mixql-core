//Use if sbt-antlr4 plugin with snapshot version is used

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/releases"
)

addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.1")
addSbtPlugin("org.scoverage"       % "sbt-scoverage"    % "2.0.8")
addSbtPlugin("org.mixql"           % "sbt-antlr4"       % "0.8.7")
addSbtPlugin("org.scalameta"       % "sbt-scalafmt"     % "2.4.6")
addSbtPlugin("org.xerial.sbt"      % "sbt-sonatype"     % "3.9.15")
addSbtPlugin("com.github.sbt"      % "sbt-pgp"          % "2.1.2")

//For testing only
//addSbtPlugin("com.eed3si9n"   % "sbt-assembly"        % "1.0.0")
//addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.11")
