import Dependencies._
import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.1-SNAPSHOT"
ThisBuild / organization     := "com.ideal.linked"

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-deduction-common",
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.1.0",
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.1-SNAPSHOT",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.14",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.31",
    libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test,
    libraryDependencies += "com.ideal.linked" %% "toposoid-sentence-transformer-neo4j" % "0.1-SNAPSHOT" % Test

  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
