import Dependencies._
import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.5"
ThisBuild / organization     := "com.ideal.linked"

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-deduction-common",
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.5",
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.5",
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.5",
    libraryDependencies += "com.ideal.linked" %% "toposoid-feature-vectorizer" % "0.5",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.15",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.31",
    libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
    libraryDependencies += "com.ideal.linked" %% "toposoid-sentence-transformer-neo4j" % "0.5" % Test,
    libraryDependencies += "io.jvm.uuid" %% "scala-uuid" % "0.3.1" % Test
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
