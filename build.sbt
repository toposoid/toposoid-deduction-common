import Dependencies._
import sbt.Keys.libraryDependencies
import de.heikoseeberger.sbtheader.License

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.6"
ThisBuild / organization     := "com.ideal.linked"

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-deduction-common",
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.6",
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.6",
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.6",
    libraryDependencies += "com.ideal.linked" %% "toposoid-feature-vectorizer" % "0.6",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.15",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.31",
    libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
    libraryDependencies += "com.ideal.linked" %% "toposoid-test-utils" % "0.6" % Test,
    libraryDependencies += "io.jvm.uuid" %% "scala-uuid" % "0.3.1" % Test
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("AGPL-3.0-or-later", new URL("http://www.gnu.org/licenses/agpl-3.0.en.html"))
headerLicense := Some(License.AGPLv3("2025", organizationName.value))
