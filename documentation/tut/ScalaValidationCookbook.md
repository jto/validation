# Cookbook

> All the examples below are validating Json objects. The API is not dedicated only to Json, it can be used on any type. Please refer to [Validating Json](ScalaValidationJson.md), [Validating Forms](ScalaValidationMigrationForm.md), and [Supporting new types](ScalaValidationExtensions.md) for more information.

## `Rule`

### Typical case class validation

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> case class Creature(
     |   name: String,
     |   isDead: Boolean,
     |   weight: Float)
defined class Creature

scala> implicit val creatureRule = From[JsValue]{ __ =>
     |   import jto.validation.playjson.Rules._
     |   ((__ \ "name").read[String] ~
     |    (__ \ "isDead").read[Boolean] ~
     |    (__ \ "weight").read[Float]) (Creature.apply)
     | }
creatureRule: jto.validation.Rule[play.api.libs.json.JsValue,Creature] = jto.validation.Rule$$anon$3@6b53e74d

scala> val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0f)
js: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1}

scala> From[JsValue, Creature](js)
res0: jto.validation.VA[Creature] = Valid(Creature(gremlins,false,1.0))

scala> From[JsValue, Creature](Json.obj())
res1: jto.validation.VA[Creature] = Invalid(List((/name,List(ValidationError(List(error.required),WrappedArray()))), (/isDead,List(ValidationError(List(error.required),WrappedArray()))), (/weight,List(ValidationError(List(error.required),WrappedArray())))))
```

### Dependent values

A common example of this use case is the validation of `password` and `password confirmation` fields in a signup form.

1. First, you need to validate that each field is valid independently
2. Then, given the two values, you need to validate that they are equals.

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> val passRule = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     |   // This code creates a `Rule[JsValue, (String, String)]` each of of the String must be non-empty
     |   ((__ \ "password").read(notEmpty) ~
     |    (__ \ "verify").read(notEmpty)).tupled
     |    	// We then create a `Rule[(String, String), String]` validating that given a `(String, String)`,
     |    	// both strings are equals. Those rules are then composed together.
     |     .compose(Rule.uncurry(json.Rules.equalTo[String])
     |     // In case of `Invalid`, we want to control the field holding the errors.
     |     // We change the `Path` of errors using `repath`
     |     .repath(_ => (Path \ "verify")))
     | }
passRule: jto.validation.Rule[play.api.libs.json.JsValue,String] = jto.validation.Rule$$anon$3@1873decb
```

Let's test it:

```scala
scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> "foo"))
res2: jto.validation.VA[String] = Valid(foo)

scala> passRule.validate(Json.obj("password" -> "", "verify" -> "foo"))
res3: jto.validation.VA[String] = Invalid(List((/password,List(ValidationError(List(error.required),WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> ""))
res4: jto.validation.VA[String] = Invalid(List((/verify,List(ValidationError(List(error.required),WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "", "verify" -> ""))
res5: jto.validation.VA[String] = Invalid(List((/password,List(ValidationError(List(error.required),WrappedArray()))), (/verify,List(ValidationError(List(error.required),WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> "bar"))
res6: jto.validation.VA[String] = Invalid(List((/verify,List(ValidationError(List(error.equals),WrappedArray(foo))))))
```

### Recursive types

When validating recursive types:

- Use the `lazy` keyword to allow forward reference.
- As with any recursive definition, the type of the `Rule` **must** be explicitly given.

```scala
scala> case class User(
     |   name: String,
     |   age: Int,
     |   email: Option[String],
     |   isAlive: Boolean,
     |   friend: Option[User])
defined class User
```

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> // Note the lazy keyword, and the explicit typing
     | implicit lazy val userRule: Rule[JsValue, User] = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     | 
     |   ((__ \ "name").read[String] ~
     |    (__ \ "age").read[Int] ~
     |    (__ \ "email").read[Option[String]] ~
     |    (__ \ "isAlive").read[Boolean] ~
     |    (__ \ "friend").read[Option[User]]) (User.apply)
     | }
userRule: jto.validation.Rule[play.api.libs.json.JsValue,User] = <lazy>
```

or using macros:

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> import jto.validation.playjson.Rules._
import jto.validation.playjson.Rules._

scala> // Note the lazy keyword, and the explicit typing
     | implicit lazy val userRule: Rule[JsValue, User] = Rule.gen[JsValue, User]
userRule: jto.validation.Rule[play.api.libs.json.JsValue,User] = <lazy>
```

### Read keys

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> val js = Json.parse("""
     | {
     |   "values": [
     |     { "foo": "bar" },
     |     { "bar": "baz" }
     |   ]
     | }
     | """)
js: play.api.libs.json.JsValue = {"values":[{"foo":"bar"},{"bar":"baz"}]}

scala> val r = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     | 
     |   val tupleR = Rule.fromMapping[JsValue, (String, String)] {
     |     case JsObject(Seq((key, JsString(value)))) => Valid(key.toString -> value)
     |     case _ => Invalid(Seq(ValidationError("BAAAM")))
     |   }
     | 
     |   (__ \ "values").read(seqR(tupleR))
     | }
r: jto.validation.Rule[play.api.libs.json.JsValue,Seq[(String, String)]] = jto.validation.Rule$$anon$3@74329825

scala> r.validate(js)
res9: jto.validation.VA[Seq[(String, String)]] = Invalid(List((/values[0],List(ValidationError(List(BAAAM),WrappedArray()))), (/values[1],List(ValidationError(List(BAAAM),WrappedArray())))))
```

### Validate subclasses (and parse the concrete class)

Consider the following class definitions:

```scala
scala> trait A
defined trait A

scala> case class B(foo: Int) extends A
defined class B

scala> case class C(bar: Int) extends A
defined class C

scala> val b = Json.obj("name" -> "B", "foo" -> 4)
b: play.api.libs.json.JsObject = {"name":"B","foo":4}

scala> val c = Json.obj("name" -> "C", "bar" -> 6)
c: play.api.libs.json.JsObject = {"name":"C","bar":6}

scala> val e = Json.obj("name" -> "E", "eee" -> 6)
e: play.api.libs.json.JsObject = {"name":"E","eee":6}
```

#### Trying all the possible rules implementations

```scala
scala> val rb: Rule[JsValue, A] = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     |   (__ \ "name").read(json.Rules.equalTo("B")) *> (__ \ "foo").read[Int].map(B.apply)
     | }
rb: jto.validation.Rule[play.api.libs.json.JsValue,A] = jto.validation.Rule$$anon$3@6ae4856a

scala> val rc: Rule[JsValue, A] = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     |   (__ \ "name").read(json.Rules.equalTo("C")) *> (__ \ "bar").read[Int].map(C.apply)
     | }
rc: jto.validation.Rule[play.api.libs.json.JsValue,A] = jto.validation.Rule$$anon$3@67690dc9

scala> val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
typeInvalid: cats.data.Validated.Invalid[Seq[(jto.validation.Path.type, Seq[jto.validation.ValidationError])]] = Invalid(List((/,List(ValidationError(List(validation.unknownType),WrappedArray())))))

scala> val rule = rb orElse rc orElse Rule(_ => typeInvalid)
rule: jto.validation.Rule[play.api.libs.json.JsValue,A] = jto.validation.Rule$$anon$2@1d368518

scala> rule.validate(b)
res10: jto.validation.VA[A] = Valid(B(4))

scala> rule.validate(c)
res11: jto.validation.VA[A] = Valid(C(6))

scala> rule.validate(e)
res12: jto.validation.VA[A] = Invalid(List((/,List(ValidationError(List(validation.unknownType),WrappedArray())))))
```

#### Using class discovery based on field discrimination

```scala
scala> val typeInvalid = Invalid(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
typeInvalid: cats.data.Validated.Invalid[Seq[(jto.validation.Path.type, Seq[jto.validation.ValidationError])]] = Invalid(List((/,List(ValidationError(List(validation.unknownType),WrappedArray())))))

scala> val rule = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     | 	(__ \ "name").read[String].flatMap[A] {
     | 	  case "B" => (__ \ "foo").read[Int].map(B.apply)
     | 	  case "C" => (__ \ "bar").read[Int].map(C.apply)
     | 	  case _ => Rule(_ => typeInvalid)
     | 	}
     | }
rule: jto.validation.Rule[play.api.libs.json.JsValue,A] = jto.validation.Rule$$anon$3@4db6ff2e

scala> rule.validate(b)
res13: jto.validation.VA[A] = Valid(B(4))

scala> rule.validate(c)
res14: jto.validation.VA[A] = Valid(C(6))

scala> rule.validate(e)
res15: jto.validation.VA[A] = Invalid(List((/,List(ValidationError(List(validation.unknownType),WrappedArray())))))
```

## `Write`

### typical case class `Write`

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> case class Creature(
     |   name: String,
     |   isDead: Boolean,
     |   weight: Float)
defined class Creature

scala> implicit val creatureWrite = To[JsObject] { __ =>
     |   import jto.validation.playjson.Writes._
     |   ((__ \ "name").write[String] ~
     |    (__ \ "isDead").write[Boolean] ~
     |    (__ \ "weight").write[Float]) (Creature.unapply)
     | }
creatureWrite: jto.validation.Write[Creature,play.api.libs.json.JsObject] = jto.validation.Write$$anon$3@3673461

scala> To[Creature, JsObject](Creature("gremlins", false, 1f))
res16: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1}
```

### Adding static values to a `Write`

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> case class LatLong(lat: Float, long: Float)
defined class LatLong

scala> implicit val latLongWrite = {
     |   import jto.validation.playjson.Writes._
     |   To[JsObject] { __ =>
     |     ((__ \ "lat").write[Float] ~
     |      (__ \ "long").write[Float]) (LatLong.unapply)
     |   }
     | }
latLongWrite: jto.validation.Write[LatLong,play.api.libs.json.JsObject] = jto.validation.Write$$anon$3@5f943fc5

scala> case class Point(coords: LatLong)
defined class Point

scala> implicit val pointWrite = {
     |   import jto.validation.playjson.Writes._
     |   To[JsObject] { __ =>
     |     ((__ \ "coords").write[LatLong] ~
     |      (__ \ "type").write[String]) ((p: Point) => Some(p.coords -> "point"))
     |   }
     | }
pointWrite: jto.validation.Write[Point,play.api.libs.json.JsObject] = jto.validation.Write$$anon$3@24ed0db8

scala> val p = Point(LatLong(123.3F, 334.5F))
p: Point = Point(LatLong(123.3,334.5))

scala> pointWrite.writes(p)
res17: play.api.libs.json.JsObject = {"coords":{"lat":123.3,"long":334.5},"type":"point"}
```
