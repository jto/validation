# Exporting Validations to Javascript using Scala.js

```tut:invisible
def cat(path: String): Unit =
  println(scala.io.Source.fromFile(s"play-scalajs-example/$path").mkString.trim)
```
Validation 2.0.x supports Scala.js, which allows compiling validation logic for JavaScript to run it directly in the browser. Let's begin by playing with it. Try to change the `tryMe` variable in the following editor. The result is automatically outputted.

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

Using validation from Scala.js is no different than any other Scala library. There is, however, some friction to integrate Scala.js into an existing Play + JavaScript, which we try to address in this document. Assuming no prior knowledge on Scala.js, we explain how to cross-compile and integrate validation logic into an existing Play/JavaScript application.

You will first need to add two SBT plugins, Scala.js itself and `sbt-play-scalajs` to make it Scala.js and Play coexist nicely:

```tut
cat("project/plugins.sbt")
```

Scala.js uses a separate compilation pass to transform Scala sources to a single `.js` file. Specifying which part of a Scala codebase should be processed by Scala.js is done by splitting the code in different SBT projects. This is usually done with 3 projects, one targeting the JVM, another one targeting JS, and a third one for code shared between the two. In case of a Play application it could look like the following:

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

Let's define a simple case class for our example inside of the `shared` project to make it available to both JVM and JV platforms. We collocate a simple validation for this case class in its companion object:

```tut
cat("shared/src/main/scala/User.scala")
```

Note the use of `jto.validation.jsonast` here. This project implements in just a few lines of code an immutable version of the JSON specification based on Scala collections: (It might eventually be replaced with an external abstract syntax tree (AST), see discussion in <https://github.com/scala/slip/pull/28>)

```tut
cat("../validation-jsonast/shared/src/main/scala/JValue.scala")
```

This AST has the same capabilities than other JSON representations, but it does no provide a parser nor a pretty printer. The suggested approach here is to use conversions from this cross compiled AST to platform specific ones to take advantage of existing platform specific serialization. To do so, Validation provides the following `Rule`s and `Write`s, defined in `jto.validation.jsonast`:

- `Ast.from: Rule[play.api.libs.json.JsValue, JValue]`
- `Ast.to:   Write[JValue, play.api.libs.json.JsValue]`
- `Ast.from: Rule[scala.scalajs.jsDynamic, JValue]`
- `Ast.to:   Write[JValue, scala.scalajs.jsDynamic]`

To use our previously defined validation, we could compose what we defined targeting the cross compiling JSON AST with the above `Rule`s / `Write`s to finally obtain platform-specific validation.

One last technicality about Scala.js is the `@JSExport` annotation, which is used to explicitly expose Scala objects and methods to the javascript world. To complete our example, we define and expose a single method taking a JSON representation of our case class and returning the output of our validation, also a JSON:

```tut
cat("js/src/main/scala/Validate.scala")
```

Finally, we can create a simple view with a textarea which validates it's content on every keystroke:

```tut
cat("jvm/app/views/index.scala.html")
```

This complete code of this example is available in the [play-scalajs-example](https://github.com/jto/validation/tree/v2.0/play-scalajs-example) subproject. The binary used to power the editor at the beginning of this page was generated by running Play in production mode, which fully optimizes the output of Scala.js compilation using the Google Closure Compiler to obtain a final .js file under 100KB once gzipped.
