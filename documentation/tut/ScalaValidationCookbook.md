# Cookbook

> All the examples below are validating Json objects. The API is not dedicated only to Json, it can be used on any type. Please refer to [Validating Json](ScalaValidationJson.md), [Validating Forms](ScalaValidationMigrationForm.md), and [Supporting new types](ScalaValidationExtensions.md) for more information.

## Rule

### Typical case class validation

```scala
scala> 	import play.api.libs.json._, play.api.data.mapping._
import play.api.libs.json._
import play.api.data.mapping._

scala>   case class Creature(
     |     name: String,
     |     isDead: Boolean,
     |     weight: Float)
defined class Creature

scala>   implicit val creatureRule = From[JsValue]{ __ =>
     |     import play.api.data.mapping.json.Rules._
     |     (
     |       (__ \ "name").read[String] ~
     |       (__ \ "isDead").read[Boolean] ~
     |       (__ \ "weight").read[Float]
     |     )(Creature.apply _)
     |   }
creatureRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,Creature] = play.api.data.mapping.Rule$$anon$2@2cdc3127

scala>   val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0f)
js: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1.0}

scala>   From[JsValue, Creature](js)
res0: play.api.data.mapping.VA[Creature] = Success(Creature(gremlins,false,1.0))

scala>   From[JsValue, Creature](Json.obj())
res1: play.api.data.mapping.VA[Creature] = Failure(List((/name,List(ValidationError(error.required,WrappedArray()))), (/isDead,List(ValidationError(error.required,WrappedArray()))), (/weight,List(ValidationError(error.required,WrappedArray())))))
```

### Dependent values

A common example of this use case is the validation of `password` and `password confirmation` fields in a signup form.

1. First, you need to validate that each field is valid independently
2. Then, given the two values, you need to validate that they are equals.

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> import play.api.libs.functional._
import play.api.libs.functional._

scala> import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._

scala> import play.api.data.mapping._
import play.api.data.mapping._

scala> val passRule = From[JsValue] { __ =>
     |   import play.api.data.mapping.json.Rules, Rules._
     |   // This code creates a `Rule[JsValue, (String, String)]` each of of the String must be non-empty
     |   ((__ \ "password").read(notEmpty) ~
     |    (__ \ "verify").read(notEmpty)).tupled
     |    	// We then create a `Rule[(String, String), String]` validating that given a `(String, String)`,
     |    	// both strings are equals. Those rules are then composed together.
     |     .compose(Rule.uncurry(Rules.equalTo[String])
     |     // In case of `Failure`, we want to control the field holding the errors.
     |     // We change the `Path` of errors using `repath`
     |     .repath(_ => (Path \ "verify")))
     | }
passRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,String] = play.api.data.mapping.Rule$$anon$2@56b1e588
```

Let's test it:

```scala
scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> "foo"))
res2: play.api.data.mapping.VA[String] = Success(foo)

scala> passRule.validate(Json.obj("password" -> "", "verify" -> "foo"))
res3: play.api.data.mapping.VA[String] = Failure(List((/password,List(ValidationError(error.required,WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> ""))
res4: play.api.data.mapping.VA[String] = Failure(List((/verify,List(ValidationError(error.required,WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "", "verify" -> ""))
res5: play.api.data.mapping.VA[String] = Failure(List((/password,List(ValidationError(error.required,WrappedArray()))), (/verify,List(ValidationError(error.required,WrappedArray())))))

scala> passRule.validate(Json.obj("password" -> "foo", "verify" -> "bar"))
res6: play.api.data.mapping.VA[String] = Failure(List((/verify,List(ValidationError(error.equals,WrappedArray(foo))))))
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
scala> import play.api.libs.json._, play.api.data.mapping._
import play.api.libs.json._
import play.api.data.mapping._

scala> // Note the lazy keyword, and the explicit typing
     | implicit lazy val userRule: Rule[JsValue, User] = From[JsValue] { __ =>
     |   import play.api.data.mapping.json.Rules._
     |   ((__ \ "name").read[String] and
     |    (__ \ "age").read[Int] and
     |    (__ \ "email").read[Option[String]] and
     |    (__ \ "isAlive").read[Boolean] and
     |    (__ \ "friend").read[Option[User]])(User.apply _)
     | }
userRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,User] = <lazy>
```

or using macros:

```scala
scala> import play.api.libs.json._, play.api.data.mapping._, play.api.data.mapping.json.Rules._
import play.api.libs.json._
import play.api.data.mapping._
import play.api.data.mapping.json.Rules._

scala> // Note the lazy keyword, and the explicit typing
     | implicit lazy val userRule: Rule[JsValue, User] = Rule.gen[JsValue, User]
userRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,User] = <lazy>
```

### Read keys

```scala
scala> import play.api.libs.json._, play.api.data.mapping._
import play.api.libs.json._
import play.api.data.mapping._

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
     |   import play.api.data.mapping.json.Rules._
     |   val tupleR = Rule.fromMapping[JsValue, (String, String)]{
     |     case JsObject(Seq((key, JsString(value)))) =>  Success(key -> value)
     |     case _ => Failure(Seq(ValidationError("BAAAM")))
     |   }
     |   (__ \ "values").read(seqR(tupleR))
     | }
r: play.api.data.mapping.Rule[play.api.libs.json.JsValue,Seq[(String, String)]] = play.api.data.mapping.Rule$$anon$2@3355d127

scala> r.validate(js)
res9: play.api.data.mapping.VA[Seq[(String, String)]] = Success(List((foo,bar), (bar,baz)))
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
scala> val rb: Rule[JsValue, A] = From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules, Rules._
     |   (__ \ "name").read(Rules.equalTo("B")) ~> (__ \ "foo").read[Int].fmap(B.apply _)
     | }
rb: play.api.data.mapping.Rule[play.api.libs.json.JsValue,A] = play.api.data.mapping.Rule$$anon$2@4dbfbe84

scala> val rc: Rule[JsValue, A] = From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules, Rules._
     |   (__ \ "name").read(Rules.equalTo("C")) ~> (__ \ "bar").read[Int].fmap(C.apply _)
     | }
rc: play.api.data.mapping.Rule[play.api.libs.json.JsValue,A] = play.api.data.mapping.Rule$$anon$2@7e21b559

scala> val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
typeFailure: play.api.data.mapping.Failure[(play.api.data.mapping.Path.type, Seq[play.api.data.mapping.ValidationError]),Nothing] = Failure(List((/,List(ValidationError(validation.unknownType,WrappedArray())))))

scala> val rule = rb orElse rc orElse Rule(_ => typeFailure)
rule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,A] = play.api.data.mapping.Rule$$anon$1@393ab4cf

scala> rule.validate(b)
res10: play.api.data.mapping.VA[A] = Success(B(4))

scala> rule.validate(c)
res11: play.api.data.mapping.VA[A] = Success(C(6))

scala> rule.validate(e)
res12: play.api.data.mapping.VA[A] = Failure(List((/,List(ValidationError(validation.unknownType,WrappedArray())))))
```

#### Using class discovery based on field discrimination

```scala
scala> val typeFailure = Failure(Seq(Path -> Seq(ValidationError("validation.unknownType"))))
typeFailure: play.api.data.mapping.Failure[(play.api.data.mapping.Path.type, Seq[play.api.data.mapping.ValidationError]),Nothing] = Failure(List((/,List(ValidationError(validation.unknownType,WrappedArray())))))

scala> val rule = From[JsValue] { __ =>
     | 	import play.api.data.mapping.json.Rules._
     | 	(__ \ "name").read[String].flatMap[A] {
     | 	  case "B" => (__ \ "foo").read[Int].fmap(B.apply _)
     | 	  case "C" => (__ \ "bar").read[Int].fmap(C.apply _)
     | 	  case _ => Rule(_ => typeFailure)
     | 	}
     | }
rule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,A] = play.api.data.mapping.Rule$$anon$2@724ddc6e

scala> rule.validate(b)
res13: play.api.data.mapping.VA[A] = Success(B(4))

scala> rule.validate(c)
res14: play.api.data.mapping.VA[A] = Success(C(6))

scala> rule.validate(e)
res15: play.api.data.mapping.VA[A] = Failure(List((/,List(ValidationError(validation.unknownType,WrappedArray())))))
```

## Write

### typical case class `Write`

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> import play.api.data.mapping._
import play.api.data.mapping._

scala> import play.api.libs.functional.syntax.unlift
import play.api.libs.functional.syntax.unlift

scala> case class Creature(
     |   name: String,
     |   isDead: Boolean,
     |   weight: Float)
defined class Creature

scala> implicit val creatureWrite = To[JsObject]{ __ =>
     |   import play.api.data.mapping.json.Writes._
     |   (
     |     (__ \ "name").write[String] ~
     |     (__ \ "isDead").write[Boolean] ~
     |     (__ \ "weight").write[Float]
     |   )(unlift(Creature.unapply _))
     | }
creatureWrite: play.api.data.mapping.Write[Creature,play.api.libs.json.JsObject] = play.api.data.mapping.Write$$anon$2@7f2b5738

scala> To[Creature, JsObject](Creature("gremlins", false, 1f))
res16: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1.0}
```

### Adding static values to a `Write`

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> import play.api.libs.functional._
import play.api.libs.functional._

scala> import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._

scala> import play.api.data.mapping._
import play.api.data.mapping._

scala> case class LatLong(lat: Float, long: Float)
defined class LatLong

scala> implicit val latLongWrite = {
     |   import play.api.data.mapping.json.Writes._
     |   To[JsObject] { __ =>
     |     ((__ \ "lat").write[Float] ~
     |      (__ \ "long").write[Float])(unlift(LatLong.unapply _))
     |   }
     | }
latLongWrite: play.api.data.mapping.Write[LatLong,play.api.libs.json.JsObject] = play.api.data.mapping.Write$$anon$2@6e3825

scala> case class Point(coords: LatLong)
defined class Point

scala> implicit val pointWrite = {
     |   import play.api.data.mapping.json.Writes._
     |   To[JsObject] { __ =>
     |     ((__ \ "coords").write[LatLong] ~
     |      (__ \ "type").write[String])((_: Point).coords -> "point")
     |   }
     | }
pointWrite: play.api.data.mapping.Write[Point,play.api.libs.json.JsObject] = play.api.data.mapping.Write$$anon$2@600c42e7

scala> val p = Point(LatLong(123.3F, 334.5F))
p: Point = Point(LatLong(123.3,334.5))

scala> pointWrite.writes(p)
res17: play.api.libs.json.JsObject = {"coords":{"lat":123.3,"long":334.5},"type":"point"}
```





