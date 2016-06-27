# [The Play data validation library](https://github.com/jto/validation)

## Overview

The Play validation API aims to provide a comprehensive toolkit to validate data from any format against user defined rules, and transform them to other types.

Basically, assuming you have this:

```tut:silent
case class Person(
  name: String,
  age: Int,
  lovesChocolate: Boolean
)
```

```tut:silent
import play.api.libs.json._
import jto.validation.Rule

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

```tut
personRule.validate(json)
```

It's also a unification of the [Form Validation API](https://www.playframework.com/documentation/2.3.x/ScalaForms), and the [Json validation API](https://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators).

Being based on the same concepts as the Json validation API available in previous versions, it should feel very similar to any developer already working with it. The validation API is, rather than a totally new design, a simple generalization of those concepts.

## Design

The validation API is designed around a core defined in package `play.api.data.mapping`, and "extensions". Each extension provides primitives to validate and serialize data from / to a particular format ([Json](ScalaValidationJson.md), [form encoded request body](ScalaValidationMigrationForm.md), etc.). See [the extensions documentation](ScalaValidationExtensions.md) for more information.

To learn more about data validation, please consult [Validation and transformation with Rule](ScalaValidationRule.md), for data serialization read [Serialization with Write](ScalaValidationWrite.md). If you just want to figure all this out by yourself, please see the [Cookbook](ScalaValidationCookbook.md).
