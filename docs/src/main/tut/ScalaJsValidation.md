```tut:invisible
def cat(path: String): Unit = {
  val url = s"https://raw.githubusercontent.com/OlivierBlanvillain/play-scalajs-validation-example/master/$path"
  println(scala.io.Source.fromURL(url).mkString.trim)
}
```
Validation 2.0 supports Scala.js, which allows compiling validation logic for JavaScript. This document explains how to integrate this feature in an existing Play/JavaScript application, assuming no prior knowledge on Scala.js.

You will first need to add two sbt plugins, Scala.js itself and `sbt-play-scalajs` to make it Scala.js and Play coexist nicely:

```tut
cat("project/plugins.sbt")
```

Scala.js uses a separate compilation pass to transforms Scala sources in a single `.js` file. To indicate which part the Scala  codebase should be considered by Scala.js, you will need to use a separate sbt project. This is usually done with 3 projects, one targeting the JVM, another one targeting JS, and a third one for code shared between the two. In case of a Play application it could look like the following:

```
<project root>
 +- build.sbt
 +- jvm
 |   +- app
 |   +- conf
 |   +- public
 |   +- test
 +- js
 |   +- src/main/scala
 +- shared
     +- src/main/scala
```

Now let's look at a minimal `build.sbt` reflecting this structure. Information on the sbt settings are available on the [Scala.js documentation on cross build](https://www.scala-js.org/doc/project/cross-build.html), and on [`sbt-play-scalajs` documentation](https://github.com/vmunier/sbt-play-scalajs).

```tut
cat("build.sbt")
```

In addition to the `validation` dependency, we also included `play-scalajs-scripts`, which provides a convenient way to link the output of Scala.js compilation from a Play template:

```tut
cat("jvm/app/views/main.scala.html")
```

Let's define a simple case class for our example, inside of the `shared` project to make it available to both JVM and JV platforms:

```tut
cat("shared/src/main/scala/User.scala")
```

In order to coexist with an existing JavaScript codebase, we have explicitly exposed some objects and methods to make then callable from JavaScript, which is done with the `@JSExport` annotation. This example exposes a single method taking a JavaScript object and returning a String representation of the validation output:

```tut
cat("js/src/main/scala/Validate.scala")
```

As an example, we create a simple view with a textarea which validates it's content on every keystroke:

```tut
cat("jvm/app/views/index.scala.html")
```

This complete example is available in a [separate repository](https://github.com/OlivierBlanvillain/play-scalajs-validation-example). Since everything is happening on the client side, you also try out the result on a static version served by [GitHub pages](https://olivierblanvillain.github.io/play-scalajs-validation-example/). This page was generated with Play in production mode, which fully optimizes the output of Scala.js compilation using the Google Closure Compiler, resulting a binary file under 100KB gzipped.
