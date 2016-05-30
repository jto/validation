```tut:invisible
def cat(path: String): Unit =
  println(scala.io.Source.fromFile(s"play-scalajs-example/$path").mkString.trim)
```
Validation 2.0 supports Scala.js, which allows compiling validation logic for JavaScript to run it directly in the browser. Let's begin by playing with it. Try to change the `tryMe` variable in the following editor. The result is automatically output.

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/codemirror.css">
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/codemirror.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/mode/javascript/javascript.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/addon/edit/matchbrackets.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/addon/selection/active-line.min.js"></script>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.15.2/theme/material.min.css">
<style type="text/css">
  .CodeMirror {
    width: 350px;
    float: left;
    height: 300px;
    font-size:13px;
    margin: 10px;
  }

  #code-form {
    overflow: auto;
  }
</style>
<form id="code-form">
<textarea id="json-form" rows="10" cols="50">
// --------
// Try editing the tryMe object
// --------
var tryMe = {
  "name" : "supercat",
  "age" : 20,
  "email" : "e@mail.com",
  "isAlive" : true
};
client.Validate().user(tryMe);
</textarea>
<textarea name="" id="validation-output" cols="40" rows="50"></textarea>
</form>

<script src="js-opt.js" type="text/javascript"></script>
<script src="js-launcher.js" type="text/javascript"></script>

<script type="text/javascript">
(function() {
  var jsonFormTextarea = document.getElementById("json-form")
  var editor = CodeMirror.fromTextArea(jsonFormTextarea, {
    lineNumbers: true,
    styleActiveLine: true,
    matchBrackets: true,
    theme: "material",
    mode: {
      name: "javascript",
      json: true
    }
  });

  var output = document.getElementById("validation-output")
  var editorOutput = CodeMirror.fromTextArea(output, {
    lineNumbers: true,
    styleActiveLine: true,
    matchBrackets: true,
    theme: "material",
    readonly: true,
    mode: {
      name: "javascript",
      json: true
    }
  });

  var demo = function(jsString) {
    try {
      var jsOut = eval(jsString);
      var out = JSON.stringify(jsOut, null, 2);
      editorOutput.setValue(out);
    } catch(err) {
      editorOutput.setValue(err.message);
    }
  };

  demo(editor.getValue());

  CodeMirror.on(editor, 'changes', function(ins, obj) {
    var js = ins.getValue();
    demo(js);
  });
})()
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
