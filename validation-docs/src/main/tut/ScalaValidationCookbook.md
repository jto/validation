# Cookbook

> All the examples below are validating Json objects. The API is not dedicated only to Json, it can be used on any type. Please refer to [[Validating Json | ScalaValidationJson]], [[Validating Forms|ScalaValidationForm]], and [[Supporting new types|ScalaValidationExtensions]] for more information.

## `Rule`

### Typical case class validation

```tut
	import play.api.libs.json._, play.api.data.mapping._

  case class Creature(
    name: String,
    isDead: Boolean,
    weight: Float)

  implicit val creatureRule = From[JsValue]{ __ =>
    import play.api.data.mapping.json.Rules._
    (
      (__ \ "name").read[String] ~
      (__ \ "isDead").read[Boolean] ~
      (__ \ "weight").read[Float]
    )(Creature.apply _)
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
import play.api.libs.json._
import play.api.libs.functional._
import play.api.libs.functional.syntax._

import play.api.data.mapping._

val passRule = From[JsValue] { __ =>
  import play.api.data.mapping.json.Rules, Rules._
  // This code creates a `Rule[JsValue, (String, String)]` each of of the String must be non-empty
  ((__ \ "password").read(notEmpty) ~
   (__ \ "verify").read(notEmpty)).tupled
   	// We then create a `Rule[(String, String), String]` validating that given a `(String, String)`,
   	// both strings are equals. Those rules are then composed together.
    .compose(Rule.uncurry(Rules.equalTo[String])
    // In case of `Failure`, we want to control the field holding the errors.
    // We change the `Path` of errors using `repath`
    .repath(_ => (Path \ "verify")))
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
import play.api.libs.json._, play.api.data.mapping._

// Note the lazy keyword, and the explicit typing
implicit lazy val userRule: Rule[JsValue, User] = From[JsValue] { __ =>
  import play.api.data.mapping.json.Rules._

  ((__ \ "name").read[String] and
   (__ \ "age").read[Int] and
   (__ \ "email").read[Option[String]] and
   (__ \ "isAlive").read[Boolean] and
   (__ \ "friend").read[Option[User]])(User.apply _)
}
```

or using macros:

```tut
import play.api.libs.json._, play.api.data.mapping._, play.api.data.mapping.json.Rules._

// Note the lazy keyword, and the explicit typing
implicit lazy val userRule: Rule[JsValue, User] = Rule.gen[JsValue, User]
```

### Read keys

```tut
import play.api.libs.json._, play.api.data.mapping._

val js = Json.parse("""
{
  "values": [
    { "foo": "bar" },
    { "bar": "baz" }
  ]
}
""")

val r = From[JsValue] { __ =>
  import play.api.data.mapping.json.Rules._

  val tupleR = Rule.fromMapping[JsValue, (String, String)]{
    case JsObject(Seq((key, JsString(value)))) =>  Success(key -> value)
    case _ => Failure(Seq(ValidationError("BAAAM")))
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
val rb: Rule[JsValue, A] = From[JsValue]{ __ =>
  import play.api.data.mapping.json.Rules, Rules._
  (__ \ "name").read(Rules.equalTo("B")) ~> (__ \ "foo").read[Int].fmap(B.apply _)
}

val rc: Rule[JsValue, A] = From[JsValue]{ __ =>
  import play.api.data.mapping.json.Rules, Rules._
  (__ \ "name").read(Rules.equalTo("C")) ~> (__ \ "bar").read[Int].fmap(C.apply _)
}

val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
val rule = rb orElse rc orElse Rule(_ => typeFailure)

rule.validate(b)
rule.validate(c)
rule.validate(e)
```

#### Using class discovery based on field discrimination

```tut
val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))

val rule = From[JsValue] { __ =>
	import play.api.data.mapping.json.Rules._
	(__ \ "name").read[String].flatMap[A] {
	  case "B" => (__ \ "foo").read[Int].fmap(B.apply _)
	  case "C" => (__ \ "bar").read[Int].fmap(C.apply _)
	  case _ => Rule(_ => typeFailure)
	}
}

rule.validate(b)
rule.validate(c)
rule.validate(e)
```

## `Write`

### typical case class `Write`

```tut
import play.api.libs.json._
import play.api.data.mapping._
import play.api.libs.functional.syntax.unlift

case class Creature(
  name: String,
  isDead: Boolean,
  weight: Float)

implicit val creatureWrite = To[JsObject]{ __ =>
  import play.api.data.mapping.json.Writes._
  (
    (__ \ "name").write[String] ~
    (__ \ "isDead").write[Boolean] ~
    (__ \ "weight").write[Float]
  )(unlift(Creature.unapply _))
}

To[Creature, JsObject](Creature("gremlins", false, 1f))
```

### Adding static values to a `Write`

```tut
import play.api.libs.json._
import play.api.libs.functional._
import play.api.libs.functional.syntax._

import play.api.data.mapping._

case class LatLong(lat: Float, long: Float)

implicit val latLongWrite = {
  import play.api.data.mapping.json.Writes._
  To[JsObject] { __ =>
    ((__ \ "lat").write[Float] ~
     (__ \ "long").write[Float])(unlift(LatLong.unapply _))
  }
}

case class Point(coords: LatLong)

implicit val pointWrite = {
  import play.api.data.mapping.json.Writes._
  To[JsObject] { __ =>
    ((__ \ "coords").write[LatLong] ~
     (__ \ "type").write[String])((_: Point).coords -> "point")
  }
}

val p = Point(LatLong(123.3F, 334.5F))
pointWrite.writes(p)
```





