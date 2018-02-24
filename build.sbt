val home = "https://github.com/jto/validation"
val repo = "git@github.com:jto/validation.git"
val org = "io.github.jto"
val license =
  ("Apache License", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

val catsVersion = "1.0.0-RC1"
val jodaConvertVersion = "1.9.2"
val jodaTimeVersion = "2.9.9"
val kindProjectorVersion = "0.9.4"
val parserCombinatorsVersion = "1.0.5"
val playVersion = "2.6.7"
val scalacVersion = "2.12.4"
val scalatestVersion = "3.2.0-SNAP7"
val scalaXmlVersion = "1.0.6"

lazy val root =
  aggregate("validation", validationJVM, validationJS, docs).in(file("."))
lazy val validationJVM = aggregate("validationJVM",
                                   coreJVM,
                                   formJVM,
                                   delimitedJVM,
                                   jsonAstJVM,
                                   `validation-playjson`,
                                   `validation-xml`,
                                   `date-tests`)
lazy val validationJS = aggregate("validationJS",
                                  coreJS,
                                  formJS,
                                  delimitedJS,
                                  jsonAstJS,
                                  `validation-jsjson`)

lazy val `validation-core` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .settings(generateBoilerplate: _*)
lazy val coreJVM = `validation-core`.jvm
lazy val coreJS = `validation-core`.js
lazy val core = aggregate("validation-core", coreJVM, coreJS)

lazy val `validation-form` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .jvmSettings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion)
  .jsSettings(libraryDependencies +=
    "org.scala-lang.modules" %%% "scala-parser-combinators" % parserCombinatorsVersion)
  .dependsOn(`validation-core`)
  .dependsOn(`validation-core` % "test->test")
lazy val formJVM = `validation-form`.jvm
lazy val formJS = `validation-form`.js
lazy val form = aggregate("validation-form", formJVM, formJS)

lazy val `validation-delimited` = crossProject
  .crossType(CrossType.Pure)
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)
lazy val delimitedJVM = `validation-delimited`.jvm
lazy val delimitedJS = `validation-delimited`.js
lazy val delimited =
  aggregate("validation-delimited", delimitedJVM, delimitedJS)

lazy val `validation-jsonast` = crossProject
  .crossType(CrossType.Full)
  .settings(validationSettings: _*)
  .dependsOn(`validation-core`)
  .dependsOn(`validation-core` % "test->test")
  .jvmSettings(
    libraryDependencies += "com.typesafe.play" %% "play-json" % playVersion)
lazy val jsonAstJVM = `validation-jsonast`.jvm
lazy val jsonAstJS = `validation-jsonast`.js
lazy val jsonAst = aggregate("validation-jsonast", jsonAstJVM, jsonAstJS)

lazy val `validation-playjson` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "com.typesafe.play" %% "play-json" % playVersion)
  .dependsOn(coreJVM)
  .dependsOn(coreJVM % "test->test")

lazy val `validation-openapi` = project
  .settings(validationSettings: _*)
  .settings(
    libraryDependencies ++= List(
      "io.swagger" % "swagger-core" % "2.0.0-rc2",
      "io.swagger" % "swagger-models" % "2.0.0-rc2"
    ))
  .dependsOn(coreJVM)
  .dependsOn(coreJVM % "test->test")

lazy val `validation-xml` = project
  .settings(validationSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion)
  .dependsOn(coreJVM)
  .dependsOn(coreJVM % "test->test")

lazy val `validation-jsjson` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(validationSettings: _*)
  .dependsOn(coreJS)
  .dependsOn(coreJS % "test->test")

lazy val docs = project
  .enablePlugins(TutPlugin)
  .settings(validationSettings: _*)
  .settings(dontPublish: _*)
  .settings(crossTarget := file(".") / "docs" / "target")
  .settings(scalacOptions -= "-Ywarn-unused-import")
  .dependsOn(coreJVM,
             formJVM,
             delimitedJVM,
             jsonAstJVM,
             `validation-playjson`,
             `validation-xml`,
             `validation-openapi`)

lazy val `date-tests` = project
  .settings(validationSettings: _*)
  .settings(dontPublish: _*)
  .dependsOn(coreJVM,
             formJVM,
             jsonAstJVM,
             `validation-playjson`,
             `validation-xml`)

def aggregate(name: String, projects: ProjectReference*): Project =
  Project(name, file("." + name))
    .aggregate(projects: _*)
    .settings(validationSettings: _*)
    .settings(dontPublish: _*)

lazy val validationSettings = settings ++ dependencies ++ doPublish ++ scoverageSettings

lazy val settings = Seq(
  scalaVersion := scalacVersion,
  crossScalaVersions := Seq("2.11.8"),
  organization := org,
  scalacOptions ++= Scalac.commonOptions,
  scalacOptions in (Compile, console) --= Scalac.consoleOptionsExcludes,
  scalacOptions in (Test, console) --= Scalac.consoleOptionsExcludes,
  scalacOptions in Test --= Scalac.consoleOptionsExcludes,
  resolvers ++= commonResolvers,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  scalafmtOnCompile := true
)

val commonResolvers = Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "com.chuusai" %%% "shapeless" % "2.3.2",
    "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
    "joda-time" % "joda-time" % jodaTimeVersion,
    "org.joda" % "joda-convert" % jodaConvertVersion
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion)
)

val generateBoilerplate = Seq(
  sourceGenerators in Compile +=
    (sourceManaged in Compile).map(Boilerplate.gen).taskValue
)

val doPublish = Seq(
  homepage := Some(url(home)),
  scmInfo := Some(ScmInfo(url(home), "scm:git:" + repo)),
  licenses := Seq(license),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (<developers>
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
