# The unified data validation library

[![Join the chat at https://gitter.im/jto/validation](https://badges.gitter.im/jto/validation.svg)](https://gitter.im/jto/validation?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Overview

The unified validation API aims to provide a comprehensive toolkit to validate data from any format against user defined rules, and transform them to other types.

Basically, assuming you have this:

```scala
import play.api.libs.json._
import play.api.data.mapping._

case class Person(name: String, age: Int, lovesChocolate: Boolean)

val json = Json.parse("""{
  "name": "Julien",
  "age": 28,
  "lovesChocolate": true
}""")

implicit val personRule = {
  import play.api.data.mapping.json.Rules._
  Rule.gen[JsValue, Person]
}
```

It can do this:

```scala
scala> personRule.validate(json)
res0: play.api.data.mapping.VA[Person] = Success(Person(Julien,28,true))
```

> **BUT IT'S NOT LIMITED TO JSON**

It's also a unification of play's [Form Validation API](https://www.playframework.com/documentation/2.3.x/ScalaForms), and its [Json validation API](https://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators).

Being based on the same concepts as play's Json validation API, it should feel very similar to any developer already working with it. The unified validation API is, rather than a totally new design, a simple generalization of those concepts.

## Design

The unified validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([Json](documentation/tut/ScalaValidationJson.md), [form encoded request body](documentation/tut/ScalaValidationMigrationForm.md), etc.). See [the extensions documentation](documentation/tut/ScalaValidationExtensions.md) for more information.

To learn more about data validation, please consult [Validation and transformation with Rule](documentation/tut/ScalaValidationRule.md), for data serialization read [Serialization with Write](documentation/tut/ScalaValidationWrite.md). If you just want to figure all this out by yourself, please see the [Cookbook](documentation/tut/ScalaValidationCookbook.md).

## Using the validation api in your project

Add the following lines in your `build.sbt`

```scala
resolvers += Resolver.sonatypeRepo("releases")

// If you want only the core
libraryDependencies +="io.github.jto" %% "validation-core" % "1.1"

// Json validation for play-json
libraryDependencies +="io.github.jto" %% "validation-json" % "1.1"

// Json validation for json4s
libraryDependencies += "io.github.jto" %% "validation-json4s" % "1.1"

// Form validation
libraryDependencies +="io.github.jto" %% "validation-form" % "1.1"

// XML validation
libraryDependencies +="io.github.jto" %% "validation-xml % "1.1"
```

## Play dependency

The 1.1.X versions are builded with play 2.4, if you are using play 2.3 you need to use the 1.0.2 version.

## Documentation

All the required documentation is directly readable from Github: https://github.com/jto/validation/tree/master/documentation/tut

- [Validating and transforming data](documentation/tut/ScalaValidationRule.md)
- [Combining Rules](documentation/tut/ScalaValidationRuleCombinators.md)
- [Validation Inception](documentation/tut/ScalaValidationMacros.md)
- [Validating Json](documentation/tut/ScalaValidationJson.md)
- [Serializing data with Write](documentation/tut/ScalaValidationWrite.md)
- [Combining Writes](documentation/tut/ScalaValidationWriteCombinators.md)
- [Play's Form API migration](documentation/tut/ScalaValidationMigrationForm.md)
- [Play's Json API migration](documentation/tut/ScalaValidationMigrationJson.md)
- [Extensions: Supporting new types](documentation/tut/ScalaValidationExtensions.md)
- [Cookbook](documentation/tut/ScalaValidationCookbook.md)

## Contributors

- Julien Tournay - http://jto.github.io
- Nick - https://github.com/stanch
- Ian Hummel - https://github.com/themodernlife
- Arthur Gautier - https://github.com/baloo
- Jacques B - https://github.com/Timshel
- Alexandre Tamborrino - https://github.com/atamborrino
