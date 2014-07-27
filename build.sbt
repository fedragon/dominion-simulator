name := "dominion-simulator"

version := "0.1.0"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "provided",
  "com.github.julien-truffaut"  %%  "monocle-core"    % "0.4.0",
  "com.github.julien-truffaut"  %%  "monocle-generic" % "0.4.0",
  "com.github.julien-truffaut"  %%  "monocle-macro"   % "0.4.0",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)
