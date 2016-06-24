val scalaV = "2.11.8"

val validationVersion = "2.0"

lazy val jvm = project
  .in(file("jvm"))
  .settings(
    scalaVersion := scalaV,
    scalaJSProjects := Seq(js),
    pipelineStages := Seq(scalaJSProd),
    libraryDependencies ++= Seq(
      "com.vmunier"   %% "play-scalajs-scripts" % "0.5.0",
      "io.github.jto" %% "validation-core"      % validationVersion,
      "io.github.jto" %% "validation-playjson"  % validationVersion,
      "io.github.jto" %% "validation-jsonast"   % validationVersion))
  .enablePlugins(PlayScala)
  .aggregate(js)
  .dependsOn(sharedJVM)

lazy val js = project
  .in(file("js"))
  .settings(
    scalaVersion := scalaV,
    persistLauncher := true,
    libraryDependencies ++= Seq(
      "io.github.jto" %%% "validation-core"    % validationVersion,
      "io.github.jto" %%% "validation-jsjson"  % validationVersion,
      "io.github.jto" %%% "validation-jsonast" % validationVersion))
  .enablePlugins(ScalaJSPlugin, ScalaJSPlay)
  .dependsOn(sharedJS)

lazy val sharedJVM = shared.jvm
lazy val sharedJS  = shared.js
lazy val shared    = crossProject.crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "io.github.jto" %%% "validation-core"    % validationVersion,
      "io.github.jto" %%% "validation-jsonast" % validationVersion))
  .jsConfigure(_.enablePlugins(ScalaJSPlay))

onLoad in Global := (Command.process("project jvm", _: State)) compose (onLoad in Global).value
