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
  val buildVersion = "2.0-SNAPSHOT"
  val playVersion = "2.4.3"
  val paradiseVersion = "2.1.0-M5"

  val scalaVersions = Seq(scalaVersion := "2.11.7")

  // Used by api docs generation to link back to the correct branch on GitHub, only when version is a SNAPSHOT
  val sourceCodeBranch = "master"

  val commonScalacOptions = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:reflectiveCalls",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused-import",
    "-Xfuture"
  )

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
    scalacOptions ++= commonScalacOptions,
    organization := org,
    version := buildVersion,
    ivyLoggingLevel := UpdateLogging.DownloadOnly,
    resolvers ++= Resolvers.all,
    fork in Test := true) ++ sonatypeSettings ++ tut.Plugin.tutSettings
}

object Dependencies {
  import BuildSettings._

  val specsDep = libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.9" % "test",
    "org.specs2" %% "specs2-junit" % "2.4.9" % "test") // This is needed to avoid a classpath issue on scalaz

  val macrosDep = addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

  val kindProjector = addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

  val xmlDep = libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

  val playDep = libraryDependencies += "com.typesafe.play" %% "play-json" % playVersion

  val coreDeps = libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.joda" % "joda-convert" % "1.3.1",
    "org.spire-math" %% "cats" % "0.2.0",
    "com.chuusai" %% "shapeless" % "2.2.5"
  )
}

object ValidationBuild extends Build {

  import BuildSettings._
  import Dependencies._

  lazy val docs = Project("validation-docs", file("validation-docs"))
    .settings(commonSettings: _*)
    .settings(crossTarget := file(".") / "documentation")
    .settings(scalacOptions -= "-Ywarn-unused-import")
    .dependsOn(core, json, json4s, form, xml, experimental)

  lazy val core = Project("validation-core", file("validation-core"))
    .settings(commonSettings: _*)
    .settings(coreDeps: _*)
    .settings(kindProjector: _*)
    .settings(macrosDep: _*)
    .settings(specsDep: _*)
    .settings(sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen))

  lazy val json = Project("validation-json", file("validation-json"))
    .settings(commonSettings: _*)
    .settings(playDep: _*)
    .settings(specsDep: _*)
    .dependsOn(core % "test->test;compile->compile")

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
    .settings(specsDep: _*)
    .dependsOn(core)

  lazy val root = project.in(file("."))
    .aggregate(core, json, form, delimited, xml, json4s, experimental)
    .settings(commonSettings: _*)
    .settings(scalaVersions: _*)
    .settings(publishArtifact := false)
}
