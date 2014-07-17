import sbt._
import Keys._

object Resolvers {
  val typesafeReleases = "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val all = Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    typesafeReleases)
}

object BuildSettings {
  import Resolvers._

  val org = "io.github.jto"
  val buildVersion = "1.0-SNAPSHOT"
  val playVersion = "2.3.0"
  val paradiseVersion = "2.0.0"
  
  val scalaVersions = Seq(
    scalaVersion := "2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.1"))
  
  // Used by api docs generation to link back to the correct branch on GitHub, only when version is a SNAPSHOT
  val sourceCodeBranch = "master"

  val commonSettings = scalaVersions ++ Seq(
    organization := org,
    version := buildVersion,
    ivyLoggingLevel := UpdateLogging.DownloadOnly,
    resolvers ++= all,
    fork in Test := true,
    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath + "/Documents/mvn-repo/snapshots"))))
}

object Dependencies {
  import BuildSettings._

  def only(version: (Int, Int), x: ModuleID) = libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some(`version`) =>
      Seq(x)
    case _ =>
      Seq()
  })

  val docDep = libraryDependencies +=
    "com.typesafe.play" %% "play-doc" % "1.0.3"

  val specsDep = libraryDependencies +=
    "org.specs2" %% "specs2" % "2.3.12" % "test"

  val shapelessDep = Seq(
    only((2, 10), "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full),
    only((2, 11), "com.chuusai" %% "shapeless" % "2.0.0"))

  val macrosDep = Seq(
    only((2, 10), "org.scalamacros" %% "quasiquotes" % paradiseVersion),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full))

  val coreDeps = Seq(
    only((2, 11), "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"),
    libraryDependencies ++= Seq(
      "joda-time" % "joda-time" % "2.2",
      "org.joda" % "joda-convert" % "1.3.1",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.typesafe.play" %% "play-functional" % playVersion,
      "com.typesafe.play" %% "play-json" % playVersion))
}

object ValidationBuild extends Build {

  import BuildSettings._
  import Dependencies._

  lazy val docs = Project("validation-docs", file("validation-docs"))
    .settings(Docs.settings: _*)
    .settings(docDep: _*)
    .settings(specsDep: _*)

  lazy val core = Project("validation-core", file("validation-core"))
    .settings(commonSettings: _*)
    .settings(coreDeps: _*)
    .settings(macrosDep: _*)
    .settings(specsDep: _*)

  lazy val json = Project("validation-json", file("validation-json"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val form = Project("validation-form", file("validation-form"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val delimited = Project("validation-delimited", file("validation-delimited"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val experimental = Project("validation-experimental", file("validation-experimental"))
    .settings(commonSettings: _*)
    .settings(shapelessDep: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val root = project.in(file("."))
    .aggregate(core, json, form, delimited, experimental)
    .settings(scalaVersions: _*)
    .settings(publishArtifact := false)
}
