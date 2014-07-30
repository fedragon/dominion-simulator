name := "dominion-simulator"

version := "0.1.0"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % "0.4.0",
  "com.github.julien-truffaut"  %%  "monocle-generic" % "0.4.0",
  "com.github.julien-truffaut"  %%  "monocle-macro"   % "0.4.0",
  "org.scalaz" %% "scalaz-core" % "7.0.6" % "provided",
  "org.slf4j" % "slf4j-api" % "1.7.7",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
//  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)
