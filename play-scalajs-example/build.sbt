val scalaV = "2.11.8"

lazy val jvm = project
  .in(file("jvm"))
  .settings(
    scalaVersion := scalaV,
    scalaJSProjects := Seq(js),
    pipelineStages := Seq(scalaJSProd),
    libraryDependencies += "com.vmunier" %% "play-scalajs-scripts" % "0.5.0")
  .enablePlugins(PlayScala)
  .aggregate(js)
  .dependsOn(sharedJVM)
  // To be used instead of `ProjectRef`s if compiled in isolation:
  // .settings(libraryDependencies ++= Seq(
  //   "io.github.jto" %% "validation-core" % "2.0",
  //   "io.github.jto" %% "validation-playjson" % "2.0"))
  .dependsOn(ProjectRef(file(".."), "validation-core"))
  .dependsOn(ProjectRef(file(".."), "validation-playjson"))
  .dependsOn(ProjectRef(file(".."), "validation-jsonast"))

lazy val js = project
  .in(file("js"))
  .settings(
    scalaVersion := scalaV,
    persistLauncher := true)
  .enablePlugins(ScalaJSPlugin, ScalaJSPlay)
  .dependsOn(sharedJS)
  // To be used instead of `ProjectRef`s if compiled in isolation:
  // .settings(libraryDependencies ++= Seq(
  //   "io.github.jto" %%% "validation-core" % "2.0",
  //   "io.github.jto" %%% "validation-jsjson" % "2.0"))
  .dependsOn(ProjectRef(file(".."), "validation-core"))
  .dependsOn(ProjectRef(file(".."), "validation-jsjson"))
  .dependsOn(ProjectRef(file(".."), "validation-jsonast"))

lazy val shared = crossProject.crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(scalaVersion := scalaV)
  .jsConfigure(_.enablePlugins(ScalaJSPlay))

lazy val sharedJVM = shared.jvm
lazy val sharedJS = shared.js

onLoad in Global := (Command.process("project jvm", _: State)) compose (onLoad in Global).value
