# Cookbook

> All the examples below are validating Json objects. The API is not dedicated only to Json, it can be used on any type. Please refer to [Validating Json](ScalaValidationJson.md), [Validating Forms](ScalaValidationMigrationForm.md), and [Supporting new types](ScalaValidationExtensions.md) for more information.

## `Rule`

### Typical case class validation

```tut
import jto.validation._
import play.api.libs.json._

case class Creature(
  name: String,
  isDead: Boolean,
  weight: Float)

implicit val creatureRule = From[JsValue]{ __ =>
  import jto.validation.playjson.Rules._
  ((__ \ "name").read[String] ~
   (__ \ "isDead").read[Boolean] ~
   (__ \ "weight").read[Float]) (Creature.apply _)
}

val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0f)
From[JsValue, Creature](js)

From[JsValue, Creature](Json.obj())
```

### Dependent values

A common example of this use case is the validation of `password` and `password confirmation` fields in a signup form.

1. First, you need to validate that each field is valid independently
2. Then, given the two values, you need to validate that they are equals.

```tut
import jto.validation._
import play.api.libs.json._

val passRule = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  // This code creates a `Rule[JsValue, (String, String)]` each of of the String must be non-empty
  ((__ \ "password").read(notEmpty) ~
   (__ \ "verify").read(notEmpty)).tupled
   	// We then create a `Rule[(String, String), String]` validating that given a `(String, String)`,
   	// both strings are equals. Those rules are then composed together.
    .andThen(Rule.uncurry(equalTo[String])
      // In case of `Invalid`, we want to control the field holding the errors.
      // We change the `Path` of errors using `repath`
      .repath(_ => (Path \ "verify"))
    )
}
```

Let's test it:

```tut
passRule.validate(Json.obj("password" -> "foo", "verify" -> "foo"))
passRule.validate(Json.obj("password" -> "", "verify" -> "foo"))
passRule.validate(Json.obj("password" -> "foo", "verify" -> ""))
passRule.validate(Json.obj("password" -> "", "verify" -> ""))
passRule.validate(Json.obj("password" -> "foo", "verify" -> "bar"))
```

### Recursive types

When validating recursive types:

- Use the `lazy` keyword to allow forward reference.
- As with any recursive definition, the type of the `Rule` **must** be explicitly given.

```tut
case class User(
  name: String,
  age: Int,
  email: Option[String],
  isAlive: Boolean,
  friend: Option[User])
```

```tut
import jto.validation._
import play.api.libs.json._

// Note the lazy keyword, and the explicit typing
implicit lazy val userRule: Rule[JsValue, User] = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._

  ((__ \ "name").read[String] ~
   (__ \ "age").read[Int] ~
   (__ \ "email").read[Option[String]] ~
   (__ \ "isAlive").read[Boolean] ~
   (__ \ "friend").read[Option[User]]) (User.apply _)
}
```

or using macros:

```tut
import jto.validation._
import play.api.libs.json._
import jto.validation.playjson.Rules._

// Note the lazy keyword, and the explicit typing
implicit lazy val userRule: Rule[JsValue, User] = Rule.gen[JsValue, User]
```

### Read keys

```tut
import jto.validation._
import play.api.libs.json._

val js = Json.parse("""
{
  "values": [
    { "foo": "bar" },
    { "bar": "baz" }
  ]
}
""")

val r = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._

  val tupleR = Rule.fromMapping[JsValue, (String, String)] {
    case JsObject(Seq((key, JsString(value)))) => Valid(key.toString -> value)
    case _ => Invalid(Seq(ValidationError("BAAAM")))
  }

  (__ \ "values").read(seqR(tupleR))
}

r.validate(js)
```

### Validate subclasses (and parse the concrete class)

Consider the following class definitions:

```tut
trait A
case class B(foo: Int) extends A
case class C(bar: Int) extends A

val b = Json.obj("name" -> "B", "foo" -> 4)
val c = Json.obj("name" -> "C", "bar" -> 6)
val e = Json.obj("name" -> "E", "eee" -> 6)
```

#### Trying all the possible rules implementations

```tut
val rb: Rule[JsValue, A] = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "name").read(equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply)
}

val rc: Rule[JsValue, A] = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "name").read(equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply)
}

val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
val rule = rb orElse rc orElse Rule(_ => typeInvalid)

rule.validate(b)
rule.validate(c)
rule.validate(e)
```

#### Using class discovery based on field discrimination

```tut
val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

val rule = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "name").read[String].flatMap[A] {
    case "B" => (__ \ "foo").read[Int].map(B.apply _)
    case "C" => (__ \ "bar").read[Int].map(C.apply _)
    case _ => Rule(_ => typeInvalid)
  }
}

rule.validate(b)
rule.validate(c)
rule.validate(e)
```

## `Write`

### typical case class `Write`

```tut
import jto.validation._
import play.api.libs.json._

case class Creature(
  name: String,
  isDead: Boolean,
  weight: Float)

implicit val creatureWrite = To[JsObject] { __ =>
  import jto.validation.playjson.Writes._
  ((__ \ "name").write[String] ~
   (__ \ "isDead").write[Boolean] ~
   (__ \ "weight").write[Float]).unlifted(Creature.unapply)
}

To[Creature, JsObject](Creature("gremlins", false, 1f))
```

### Adding static values to a `Write`

```tut
import jto.validation._
import play.api.libs.json._

case class LatLong(lat: Float, long: Float)

implicit val latLongWrite = {
  import jto.validation.playjson.Writes._
  To[JsObject] { __ =>
    ((__ \ "lat").write[Float] ~
     (__ \ "long").write[Float]).unlifted(LatLong.unapply)
  }
}

case class Point(coords: LatLong)

implicit val pointWrite = {
  import jto.validation.playjson.Writes._
  To[JsObject] { __ =>
    ((__ \ "coords").write[LatLong] ~
     (__ \ "type").write[String]) ((_: Point).coords -> "point")
  }
}

val p = Point(LatLong(123.3F, 334.5F))
pointWrite.writes(p)
```
