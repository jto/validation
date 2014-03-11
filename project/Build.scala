import sbt._
import Keys._

object Resolvers {
  val typesafeReleases = "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val all = Seq(
  	Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    typesafeReleases)
}

object Settings {
	import Resolvers._

	val org = "jto.github.io"
	val buildScalaVersion = "2.10.3"
	val buildVersion = "1.0-SNAPSHOT"
	val playVersion = "2.2.2"

	val paradiseVersion = "2.0.0-M3"

	val commonSettings = Seq(
    organization := org,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    ivyLoggingLevel := UpdateLogging.DownloadOnly,
    resolvers ++= all,
    fork in Test := true)

}

object Dependencies {
	import Settings._

	val specsBuild = "org.specs2" %% "specs2" % "2.1.1"
	val deps = Seq(
    "joda-time" % "joda-time" % "2.2",
    "org.joda" % "joda-convert" % "1.3.1",
    "org.scala-lang" % "scala-reflect" %  buildScalaVersion,
    "com.typesafe.play" %% "play-functional" % playVersion,
    specsBuild % "test")

	val scalaMacros = Seq(
		addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
		libraryDependencies += "org.scalamacros" % "quasiquotes" % paradiseVersion cross CrossVersion.full)
}

object ValidationBuild extends Build {

	import Settings._
	import Dependencies._

	lazy val core = project.in(file("validation-core"))
		.settings(commonSettings: _*)
		.settings(libraryDependencies := deps)
		.settings(scalaMacros: _*)

	lazy val json = project.in(file("validation-json")).dependsOn(core)
	lazy val form = project.in(file("validation-form"))
		.settings(libraryDependencies += specsBuild % "test")
		.dependsOn(core)
	lazy val root = project.in(file(".")).aggregate(core, json, form)
}