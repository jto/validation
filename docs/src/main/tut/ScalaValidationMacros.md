# Validation Inception

## Introduction

The validation API provides macro-based helpers to generate `Rule` and `Write` for case classes (or any class with a companion object providing `apply` / and `unapply` methods).

The generated code:

- is completely typesafe
- is compiled
- does not rely on runtime introspection **at all**
- is strictly equivalent to a hand-written definition

## Example

Traditionally, for a given case class `Person` we would define a `Rule` like this:

```tut
case class Person(name: String, age: Int, lovesChocolate: Boolean)
```

```tut:silent
import jto.validation._
import play.api.libs.json._

implicit val personRule = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  ((__ \ "name").read[String] ~
   (__ \ "age").read[Int] ~
   (__ \ "lovesChocolate").read[Boolean]) (Person.apply _)
}
```

Let's test it:

```tut
val json = Json.parse("""{
  "name": "Julien",
  "age": 28,
  "lovesChocolate": true
}""")

personRule.validate(json)
```

The exact same `Rule` can be generated using `Rule.gen`:

```tut:silent
import jto.validation._
import play.api.libs.json._

implicit val personRule = {
  import jto.validation.playjson.Rules._ // let's not leak implicits everywhere
  Rule.gen[JsValue, Person]
}
```

The validation result is identical :

```tut
val json = Json.parse("""{
  "name": "Julien",
  "age": 28,
  "lovesChocolate": true
}""")

personRule.validate(json)
```

Similarly we can generate a `Write`:

```tut:silent
import jto.validation._
import play.api.libs.json._

implicit val personWrite = {
  import jto.validation.playjson.Writes._ // let's no leak implicits everywhere
  Write.gen[Person, JsValue]
}
```
```tut
personWrite.writes(Person("Julien", 28, true))
```

## Known limitations

 - **Don’t override the apply method of the companion object.** The macro inspects the `apply` method to generate `Rule`/`Write`. Overloading the `apply` method creates an ambiguity the compiler will complain about.
 - **Macros only work when `apply` and `unapply` have corresponding input/output types**. This is naturally true for case classes. However if you want to validate a trait, you must implement the same `apply`/`unapply` you would have in a case class.
 - **Validation Macros accept `Option`/`Seq`/`List`/`Set` & `Map[String, _]`**. For other generic types, you'll have to test and possibly write your `Rule`/`Write` if it's not working out of the box.
