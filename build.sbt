// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license = ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "1.2.0"
val jodaConvertVersion = "2.1.1"
val jodaTimeVersion = "2.10"
val kindProjectorVersion = "0.9.7"
val parserCombinatorsVersion = "1.1.1"
val playVersion = "2.6.3"
val scalacVersion = "2.12.6"
val scalatestVersion = "3.2.0-SNAP10"
val scalaXmlVersion = "1.1.0"

lazy val root = aggregate("validation", validationJVM, validationJS, docs).in(file("."))
lazy val validationJVM = aggregate("validationJVM", coreJVM, formJVM, delimitedJVM, jsonAstJVM, `validation-playjson`, `validation-xml`, `date-tests`)
lazy val validationJS = aggregate("validationJS", coreJS, formJS, delimitedJS, jsonAstJS, `validation-jsjson`)

lazy val `validation-core` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(validationSettings: _*)
    .settings(generateBoilerplate: _*)
lazy val coreJVM = `validation-core`.jvm
lazy val coreJS = `validation-core`.js
lazy val core = aggregate("validation-core", coreJVM, coreJS)

lazy val `validation-form` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(validationSettings: _*)
    .settings(
      Test / scalacOptions -= "-Xlint",
      libraryDependencies +=  "org.scala-lang.modules" %%% "scala-parser-combinators" % parserCombinatorsVersion
    )
    .dependsOn(`validation-core`)
lazy val formJVM = `validation-form`.jvm
lazy val formJS = `validation-form`.js
lazy val form = aggregate("validation-form", formJVM, formJS)

lazy val `validation-delimited` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(validationSettings: _*)
    .dependsOn(`validation-core`)
lazy val delimitedJVM = `validation-delimited`.jvm
lazy val delimitedJS = `validation-delimited`.js
lazy val delimited = aggregate("validation-delimited", delimitedJVM, delimitedJS)

lazy val `validation-jsonast` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .settings(validationSettings: _*)
    .settings(Test / scalacOptions -= "-Xlint")
    .dependsOn(`validation-core`)
    .jvmSettings(libraryDependencies +=
    "com.typesafe.play" %% "play-json" % playVersion)
lazy val jsonAstJVM = `validation-jsonast`.jvm
lazy val jsonAstJS = `validation-jsonast`.js
lazy val jsonAst = aggregate("validation-jsonast", jsonAstJVM, jsonAstJS)

lazy val `validation-playjson` =
  project
    .settings(validationSettings: _*)
    .settings(
      Test / scalacOptions -= "-Xlint",
      libraryDependencies += "com.typesafe.play" %% "play-json" % playVersion
    )
    .dependsOn(coreJVM)

lazy val `validation-xml` =
  project
    .settings(validationSettings: _*)
    .settings(libraryDependencies +=
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion)
    .dependsOn(coreJVM)

lazy val `validation-jsjson` =
  project
    .enablePlugins(ScalaJSPlugin)
    .settings(validationSettings: _*)
    .dependsOn(coreJS)

lazy val docs =
  project
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)
    .settings(crossTarget := file(".") / "docs" / "target")
    //.settings(tutSettings: _*)
    .settings(scalacOptions -= "-Xlint")
    .dependsOn(coreJVM, formJVM, delimitedJVM, jsonAstJVM, `validation-playjson`, `validation-xml`)

lazy val `date-tests` =
  project
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)
    .settings(Test / scalacOptions -= "-Xlint")
    .dependsOn(coreJVM, formJVM, jsonAstJVM, `validation-playjson`, `validation-xml`)

def aggregate(name: String, projects: ProjectReference*): Project =
  Project(name, file("." + name))
    .aggregate(projects: _*)
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)

lazy val validationSettings = settings ++ dependencies ++ doPublish ++ scoverageSettings

lazy val settings = Seq(
  scalaVersion := scalacVersion,
  crossScalaVersions := Seq(scalacVersion, "2.11.12"),
  organization := org,
  scalacOptions ++= commonScalacOptions,
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value.filterNot(_ == "-Xfatal-warnings"),
  resolvers ++= commonResolvers,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  // "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture",
  "-Ypartial-unification"
)

val commonResolvers = Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
    "joda-time" % "joda-time" % jodaTimeVersion,
    "org.joda" % "joda-convert" % jodaConvertVersion
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion)
)

val generateBoilerplate = Seq(
  sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen)
)

val doPublish = Seq(
  homepage := Some(url(home)),
  scmInfo :=  Some(ScmInfo(url(home), "scm:git:" + repo)),
  licenses := Seq(license),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>jto</id>
        <name>Julien Tournay</name>
        <url>http://jto.github.io</url>
      </developer>
    </developers>)
)

val dontPublish = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

lazy val scoverageSettings = Seq(
  scoverage.ScoverageKeys.coverageExcludedPackages := """jto\.validation\.jsjson\..*""""
)
