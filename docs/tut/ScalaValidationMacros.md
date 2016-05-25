# Validation Inception

> This feature is experimental such as Scala Macros which are flagged experimental in Scala 2.10.0.
> If you would rather not use Scala experimental features, just stick to hand-written `Rule` / `Write` which are strictly equivalent.

## Introduction

The validation API provides macro-based helpers to generate `Rule` and `Write` for case classes (or any class with a companion object providing `apply` / and `unapply` methods).

The generated code:

- is completely typesafe
- is compiled
- does not rely on runtime introspection **at all**
- is strictly equivalent to a hand-written definition

## Example

Traditionally, for a given case class `Person` we would define a `Rule` like this:

```scala
scala> case class Person(name: String, age: Int, lovesChocolate: Boolean)
defined class Person
```

```scala
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

```scala
scala> val json = Json.parse("""{
     |   "name": "Julien",
     |   "age": 28,
     |   "lovesChocolate": true
     | }""")
json: play.api.libs.json.JsValue = {"name":"Julien","age":28,"lovesChocolate":true}

scala> personRule.validate(json)
res1: jto.validation.VA[Person] = Valid(Person(Julien,28,true))
```

The exact same `Rule` can be generated using `Rule.gen`:

```scala
import jto.validation._
import play.api.libs.json._

implicit val personRule = {
  import jto.validation.playjson.Rules._ // let's not leak implicits everywhere
  Rule.gen[JsValue, Person]
}
```

The validation result is identical :

```scala
scala> val json = Json.parse("""{
     |   "name": "Julien",
     |   "age": 28,
     |   "lovesChocolate": true
     | }""")
json: play.api.libs.json.JsValue = {"name":"Julien","age":28,"lovesChocolate":true}

scala> personRule.validate(json)
res3: jto.validation.VA[Person] = Valid(Person(Julien,28,true))
```

Similarly we can generate a `Write`:

```scala
import jto.validation._
import play.api.libs.json._

implicit val personWrite = {
  import jto.validation.playjson.Writes._ // let's no leak implicits everywhere
  Write.gen[Person, JsObject]
}
```
```scala
scala> personWrite.writes(Person("Julien", 28, true))
res5: play.api.libs.json.JsObject = {"name":"Julien","age":28,"lovesChocolate":true}
```

## Known limitations

 - **Don’t override the apply method of the companion object.** The macro inspects the `apply` method to generate `Rule`/`Write`. Overloading the `apply` method creates an ambiguity the compiler will complain about.
 - **Macros only work when `apply` and `unapply` have corresponding input/output types**. This is naturally true for case classes. However if you want to validate a trait, you must implement the same `apply`/`unapply` you would have in a case class.
 - **Validation Macros accept `Option`/`Seq`/`List`/`Set` & `Map[String, _]`**. For other generic types, you'll have to test and possibly write your `Rule`/`Write` if it's not working out of the box.