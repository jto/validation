# The unified data validation library

[![Travis](https://api.travis-ci.org/jto/validation.png?branch=master)](https://travis-ci.org/jto/validation) [![Coverage Status](https://coveralls.io/repos/github/jto/validation/badge.svg?branch=v2.0)](https://coveralls.io/github/jto/validation?branch=v2.0) [![Maven](https://img.shields.io/maven-central/v/io.github.jto/validation-core_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.jto/validation-core_2.11) [![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.8.svg)](https://www.scala-js.org) [![Gitter](https://badges.gitter.im/jto/validation.svg)](https://gitter.im/jto/validation?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Overview

The unified validation API aims to provide a comprehensive toolkit to validate data from any format against user defined rules, and transform them to other types.

Basically, assuming you have this:

```scala
import play.api.libs.json._
import jto.validation._

case class Person(name: String, age: Int, lovesChocolate: Boolean)

val json = Json.parse("""{
  "name": "Julien",
  "age": 28,
  "lovesChocolate": true
}""")

implicit val personRule = {
  import jto.validation.playjson.Rules._
  Rule.gen[JsValue, Person]
}
```

It can do this:

```scala
scala> personRule.validate(json)
res0: jto.validation.VA[Person] = Valid(Person(Julien,28,true))
```

> **BUT IT'S NOT LIMITED TO JSON**

It's also a unification of play's [Form Validation API](https://www.playframework.com/documentation/2.3.x/ScalaForms), and its [Json validation API](https://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators).

Being based on the same concepts as play's Json validation API, it should feel very similar to any developer already working with it. The unified validation API is, rather than a totally new design, a simple generalization of those concepts.


## Design

The unified validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([Json](http://jto.github.io/validation/docs/book/ScalaValidationJson.html), [form encoded request body](http://jto.github.io/validation/docs/book/ScalaValidationMigrationForm.html), etc.). See [the extensions documentation](http://jto.github.io/validation/docs/book/ScalaValidationExtensions.html) for more information.

To learn more about data validation, please consult [Validation and transformation with Rule](documentation/tut/ScalaValidationRule.md), for data serialization read [Serialization with Write](documentation/tut/ScalaValidationWrite.md). If you just want to figure all this out by yourself, please see the [Cookbook](documentation/tut/ScalaValidationCookbook.md).


## Using the validation api in your project

Add the following dependencies your `build.sbt` as needed:

```scala
resolvers += Resolver.sonatypeRepo("releases")

val validationVersion = "2.0"

libraryDependencies ++= Seq(
  "io.github.jto" %% "validation-core"      % validationVersion,
  "io.github.jto" %% "validation-playjson"  % validationVersion,
  "io.github.jto" %% "validation-jsonast"   % validationVersion,
  "io.github.jto" %% "validation-form"      % validationVersion,
  "io.github.jto" %% "validation-delimited" % validationVersion,
  "io.github.jto" %% "validation-xml"       % validationVersion
  // "io.github.jto" %%% "validation-jsjson"    % validationVersion
)
```

## Play dependencies

| Validation | Play  |
| ---------- | ----- |
| 2.0        | 2.5.3 |
| 1.1.x      | 2.4.x |
| 1.0.2      | 2.3.x |


## Documentation

[Documentation is here](http://jto.github.io/validation/docs/book/)

- [Validating and transforming data](http://jto.github.io/validation/docs/book/ScalaValidationRule.html)
- [Combining Rules](http://jto.github.io/validation/docs/book/ScalaValidationRuleCombinators.html)
- [Serializing data with Write](http://jto.github.io/validation/docs/book/ScalaValidationWrite.html)
- [Combining Writes](http://jto.github.io/validation/docs/book/ScalaValidationWriteCombinators.html)
- [Validation Inception](http://jto.github.io/validation/docs/book/ScalaValidationMacros.html)
- [Play's Form API migration](http://jto.github.io/validation/docs/book/ScalaValidationMigrationForm.html)
- [Play's Json API migration](http://jto.github.io/validation/docs/book/ScalaValidationMigrationJson.html)
- [Extensions: Supporting new types](http://jto.github.io/validation/docs/book/ScalaValidationExtensions.html)
- [Exporting Validations to Javascript using Scala.js](http://jto.github.io/validation/docs/book/ScalaJsValidation.html)
- [Cookbook](http://jto.github.io/validation/docs/book/ScalaValidationCookbook.html)
- [Release notes](http://jto.github.io/validation/docs/book/ReleaseNotes.html)
- [v2.0 Migration guide](http://jto.github.io/validation/docs/book/V2MigrationGuide.html)

## Contributors

- Julien Tournay - http://jto.github.io
- Olivier Blanvillain - https://github.com/OlivierBlanvillain
- Nick - https://github.com/stanch
- Ian Hummel - https://github.com/themodernlife
- Arthur Gautier - https://github.com/baloo
- Jacques B - https://github.com/Timshel
- Alexandre Tamborrino - https://github.com/atamborrino
