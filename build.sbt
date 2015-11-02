val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license = ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "0.2.0"
val jodaConvertVersion = "1.3.1"
val jodaTimeVersion = "2.2"
val json4sAstVersion = "4.0.0-SNAPSHOT-2"
val kindProjectorVersion = "0.6.3"
val parserCombinatorsVersion = "1.0.2"
val playVersion = "2.4.3"
val scalacVersion = "2.11.7"
val scalatestVersion = "3.0.0-M10"
val scalaXmlVersion = "1.0.5"
val shapelessVersion = "2.2.5"

lazy val root = aggregate("validation", `validation-core`, `validation-form`, `validation-delimited`, `validation-experimental`, `validation-json4s`, `validation-json`, `validation-xml`).in(file("."))

lazy val `validation-core` = project
  .settings(validationSettings: _*)
  .settings(generateBoilerplate: _*)

lazy val `validation-form` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion)
  .dependsOn(`validation-core`)

lazy val `validation-delimited` = project
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)

lazy val `validation-experimental` = project
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)

lazy val `validation-json4s` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.json4s" %% "json4s-ast" % json4sAstVersion)
  .dependsOn(`validation-core`)

lazy val `validation-json` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "com.typesafe.play" %% "play-json" % playVersion)
  .dependsOn(`validation-core` % "test->test;compile->compile")

lazy val `validation-xml` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion)
  .dependsOn(`validation-core`)

lazy val `validation-docs` = project
  .settings(validationSettings: _*)
  .settings(dontPublish: _*)
  .settings(crossTarget := file(".") / "documentation")
  .settings(tutSettings: _*)
  .settings(scalacOptions -= "-Ywarn-unused-import")
  .dependsOn(root)

def aggregate(name: String, projects: ProjectReference*): Project =
  Project(name, file("." + name))
    .aggregate(projects: _*)
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)

lazy val validationSettings = settings ++ dependencies ++ doPublish

lazy val settings = Seq(
  scalaVersion := scalacVersion,
  organization := org,
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  parallelExecution := false
)

val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
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

val commonResolvers = Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.spire-math" %% "cats" % catsVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    "joda-time" % "joda-time" % jodaTimeVersion,
    "org.joda" % "joda-convert" % jodaConvertVersion
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion)
)

val generateBoilerplate = Seq(
  sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
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
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
