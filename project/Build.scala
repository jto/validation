import sbt._
import Keys._

object Resolvers {
  val all = Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayRepo("scalaz", "releases"),
    Resolver.typesafeRepo("releases")
  )
}

object BuildSettings {
  val org = "io.github.jto"
  val buildVersion = "1.0"
  val playVersion = "2.3.0"
  val paradiseVersion = "2.0.1"

  val scalaVersions = Seq(
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.5", "2.11.6"))

  // Used by api docs generation to link back to the correct branch on GitHub, only when version is a SNAPSHOT
  val sourceCodeBranch = "master"

  val sonatypeSettings = Seq(
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    homepage := Some(url("https://github.com/jto/validation")),
    organizationHomepage := Some(url("http://jto.github.io/")),
    description := "The unified validation API",
    licenses += "The Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"),
    pomExtra := (
      <scm>
        <url>git@github.com:jto/validation.git</url>
        <connection>scm:git:git@github.com:jto/validation.git</connection>
      </scm>
      <developers>
        <developer>
          <id>jto</id>
          <name>Julien Tournay</name>
          <url>http://jto.github.io</url>
        </developer>
      </developers>)
  )

  val commonSettings = scalaVersions ++ Seq(
    organization := org,
    version := buildVersion,
    ivyLoggingLevel := UpdateLogging.DownloadOnly,
    resolvers ++= Resolvers.all,
    fork in Test := true) ++ sonatypeSettings ++ tut.Plugin.tutSettings
}

object Dependencies {
  import BuildSettings._

  def only(version: (Int, Int), x: ModuleID) = libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some(`version`) =>
      Seq(x)
    case _ =>
      Seq()
  })

  val specsDep = libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.9" % "test",
    "org.specs2" %% "specs2-junit" % "2.4.9" % "test") // This is needed to avoid a classpath issue on scalaz

  val shapelessDep = Seq(
    only((2, 10), "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full),
    only((2, 11), "com.chuusai" %% "shapeless" % "2.0.0"))

  val macrosDep = Seq(
    only((2, 10), "org.scalamacros" %% "quasiquotes" % paradiseVersion),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full))

  val xmlDep = Seq(
    only((2, 11), "org.scala-lang.modules" %% "scala-xml" % "1.0.2"))

  val coreDeps = Seq(
    only((2, 11), "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"),
    libraryDependencies ++= Seq(
      "joda-time" % "joda-time" % "2.2",
      "org.joda" % "joda-convert" % "1.3.1",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.typesafe.play" %% "play-functional" % playVersion,
      "com.typesafe.play" %% "play-json" % playVersion))

  val docDeps = libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play" % playVersion)
}

object ValidationBuild extends Build {

  import BuildSettings._
  import Dependencies._

  lazy val docs = Project("validation-docs", file("validation-docs"))
    .settings(commonSettings: _*)
    .settings(docDeps: _*)
    .settings(crossTarget := file(".") / "documentation")
    .dependsOn(core, json, json4s, form, xml, experimental)

  lazy val core = Project("validation-core", file("validation-core"))
    .settings(commonSettings: _*)
    .settings(coreDeps: _*)
    .settings(macrosDep: _*)
    .settings(specsDep: _*)

  lazy val json = Project("validation-json", file("validation-json"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val json4s = Project("validation-json4s", file("validation-json4s"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .settings(libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.10")
    .dependsOn(core)

  lazy val form = Project("validation-form", file("validation-form"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val delimited = Project("validation-delimited", file("validation-delimited"))
    .settings(commonSettings: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val xml = Project("validation-xml", file("validation-xml"))
    .settings(commonSettings: _*)
    .settings(xmlDep: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val experimental = Project("validation-experimental", file("validation-experimental"))
    .settings(commonSettings: _*)
    .settings(shapelessDep: _*)
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val root = project.in(file("."))
    .aggregate(core, json, form, delimited, xml, json4s, experimental)
    .settings(commonSettings: _*)
    .settings(scalaVersions: _*)
    .settings(publishArtifact := false)
}
