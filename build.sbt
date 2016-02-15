import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import sbt.Keys._

import scalariform.formatter.preferences._

scalaVersion := "2.11.7"

val akkaHttpV = "2.0.2"

val gitHeadCommitSha = settingKey[String]("current git commit SHA")

val commonScalariform = scalariformSettings :+ (ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveSpaceBeforeArguments, true)
  .setPreference(RewriteArrowSymbols, true))

resolvers ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayRepo("alari", "generic"),
  Resolver.jcenterRepo
)

val commons = Seq(
  organization := "ru.unicorndev",
  scalaVersion := "2.11.7",
  resolvers ++= Seq(
    "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
    "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.bintrayRepo("alari", "generic")
  ),
  gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head,
  licenses +=("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayPackageLabels := Seq("scala", "akka-http", "auth", "security", "api"),
  bintrayRepository := "generic",
  version := "0.1." + gitHeadCommitSha.value
) ++ commonScalariform

commons


lazy val `utils-http` = (project in file(".")).settings(commons: _*).settings(
  name := "utils-http",
  libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-json" % "2.4.6",
    "com.typesafe.akka" %% "akka-stream-experimental" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-experimental" % akkaHttpV
  )
)

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.jcenterRepo

//testOptions in Test += Tests.Argument("junitxml")

parallelExecution in Test := false

fork in Test := true

ivyScala := ivyScala.value map {
  _.copy(overrideScalaVersion = true)
}
