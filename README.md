# The unified data validation library

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

The unified validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([Json](documentation/tut/ScalaValidationJson.md), [form encoded request body](documentation/tut/ScalaValidationMigrationForm.md), etc.). See [the extensions documentation](documentation/tut/ScalaValidationExtensions.md) for more information.

To learn more about data validation, please consult [Validation and transformation with Rule](documentation/tut/ScalaValidationRule.md), for data serialization read [Serialization with Write](documentation/tut/ScalaValidationWrite.md). If you just want to figure all this out by yourself, please see the [Cookbook](documentation/tut/ScalaValidationCookbook.md).

## Using the validation api in your project

### 2.x branch

Add the following lines in your `build.sbt`

```scala
resolvers += Resolver.sonatypeRepo("releases")

// If you want only the core
libraryDependencies +="io.github.jto" %% "validation-core" % "2.0.0"

// Json validation based on play-json AST (optional)
libraryDependencies +="io.github.jto" %% "validation-playjson" % "2.0.0"

// Json validation based on json4s AST (optional)
libraryDependencies +="io.github.jto" %% "validation-json4s" % "2.0.0"

// Form validation (optional)
libraryDependencies +="io.github.jto" %% "validation-form" % "2.0.0"

// CSV validation (optional)
libraryDependencies +="io.github.jto" %% "validation-delimited" % "2.0.0"

// XML validation (optional)
libraryDependencies +="io.github.jto" %% "validation-xml" % "2.0.0"
```

### 1.x branch *(deprecated)*

Add the following lines in your `build.sbt`

```scala
resolvers += Resolver.sonatypeRepo("releases")

// If you want only the core
libraryDependencies +="io.github.jto" %% "validation-core" % "1.1"

// Json validation
libraryDependencies +="io.github.jto" %% "validation-json" % "1.1"

// Form validation
libraryDependencies +="io.github.jto" %% "validation-form" % "1.1"

// ...
```

## Validation 2.0.0

### Release note

- TODO: @olivier

### 1.x -> 2.x Migration guide

Version 2.x breaks back compatibility with the 1.x version. The migration has been tested on production code making heavy use of validation for json (based on play-json) and xml. Even for big project, migrating to 2.x should not take more than 30 min.

The best method is just to update validation in your dependencies, and let the compiler figure out what's broken. The following changes list should cover everything needed.

#### Build file.

The project name for play-json based validation has changed.

```scala
"io.github.jto" %% "validation-json" % validationVersion
```

becomes

```scala
"io.github.jto" %% "validation-playjson" % validationVersion
```

#### Package name

- Since the library does not depends on Play anymore, and is not planned to be integrated into Play, the package names have changed. Basically `play.api.mapping` now becomes `jto.validation`. A simple search and replace in your project should work.
- The validation api support both json4s and play-json. Therefore, the package name for play json changes. `play.api.mapping.json` becomes `play.api.mapping.playjson`

#### Rule renaming

The following `Rule` and `Write` were renamed to better match the naming convention in all subprojects.

- `Rules.jodaDate` becomes `Rules.jodaDateR`
- `Writes.jodaDate`  becomes `Writes.jodaDateW`

If you encounter implicit resolution problem, you probably have a name clash. Make sure none of your `Rule` / `Write` uses those names.

#### unlift

Since validation does not uses play-functional anymore, all the uses of `unlift` calls should disappear. We recommend you use the `unlifted` method directly.

For example the following code:

```scala
implicit lazy val w3: Write[User1, UrlFormEncoded] = To[UrlFormEncoded]{ __ =>
    ((__ \ "name").write[String] ~
     (__ \ "friend").write[Option[User1]])(unlift(User1.unapply _))
  }
```

becomes

```scala
implicit lazy val w3: Write[User1, UrlFormEncoded] = To[UrlFormEncoded]{ __ =>
    ((__ \ "name").write[String] ~
     (__ \ "friend").write[Option[User1]]).unlifted(User1.unapply _)
  }
```

The same method is also available on `Format` so

```scala
lazy val w: Format[JsValue, JsObject, RecUser] = Formatting[JsValue, JsObject]{ __ =>
    ((__ \ "name").format[String] ~
     (__ \ "friends").format(seqR(w), seqW(w)))(RecUser.apply _, unlift(RecUser.unapply _))
  }
```
becomes

```scala
lazy val w: Format[JsValue, JsObject, RecUser] = Formatting[JsValue, JsObject]{ __ =>
    ((__ \ "name").format[String] ~
     (__ \ "friends").format(seqR(w), seqW(w))).unlifted(RecUser.apply _, RecUser.unapply _)
  }
```


#### ValidationError

Since we removed all the dependencies on Play, `play.api.mapping.ValidationError` is re-defined in validation. If you're using this class, make sure to replace it by `jto.validation.ValidationError`.

## Play dependencies

- For playjson, the 2.0.0 version depends on play 2.5.3.
- 1.1.X versions are built with play 2.4
- if you are using play 2.3 you need to use the 1.0.2 version.

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
- Olivier Blanvillain - https://github.com/OlivierBlanvillain
- Nick - https://github.com/stanch
- Ian Hummel - https://github.com/themodernlife
- Arthur Gautier - https://github.com/baloo
- Jacques B - https://github.com/Timshel
- Alexandre Tamborrino - https://github.com/atamborrino
- Olivier Blanvillain - https://github.com/OlivierBlanvillain
