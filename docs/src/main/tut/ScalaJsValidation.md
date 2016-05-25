```tut:invisible
def cat(path: String): Unit = {
  val url = s"https://raw.githubusercontent.com/OlivierBlanvillain/play-scalajs-validation-example/master/$path"
  println(scala.io.Source.fromURL(url).mkString.trim)
}
```
Validation 2.0 supports Scala.js, which allows compiling validation logic for JavaScript to run it directly in the browser:

<textarea id="json-form" rows="10" cols="40">{
  "name" : "supercat",
  "age" : 20,
  "email" : "e@mail.com",
  "isAlive" : true
}</textarea><pre id="validation-output"></pre>
<script src="https://olivierblanvillain.github.io/play-scalajs-validation-example/assets/js-jsdeps.min.js" type="text/javascript"></script>
<script src="https://olivierblanvillain.github.io/play-scalajs-validation-example/assets/js-opt.js" type="text/javascript"></script>
<script src="https://olivierblanvillain.github.io/play-scalajs-validation-example/assets/js-launcher.js" type="text/javascript"></script>

<script type="text/javascript">
  var validationOutputPre = document.getElementById("validation-output")
  var jsonFormTextarea = document.getElementById("json-form")

  var demo = function() {
    try {
      var json = JSON.parse(jsonFormTextarea.value);
      validationOutputPre.innerHTML =
        JSON.stringify(client.Validate().user(json), null, 2);
    } catch(err) {
      validationOutputPre.innerHTML = err.message;
    }
  };

  jsonFormTextarea.addEventListener('input', demo, false);
  demo();
</script>

Using validation from Scala.js is no different than any other scala library. There is however some friction to intergration Scala.js into an existing Play + JavaScript, which we try to adress in this document. Assuming no prior knowledge on Scala.js, we explain how to cross compiled and integrate validation logic into an existing Play/JavaScript application.

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

This complete example is available in a [separate repository](https://github.com/OlivierBlanvillain/play-scalajs-validation-example). The example at the begining of this page was generated with Play in production mode, which fully optimizes the output of Scala.js compilation using the Google Closure Compiler, resulting a binary file under 100KB gzipped.
