# Migration from the Json API

The Json API and the new validation API are really similar. One could see the new Validation API as just an evolution of the Json API.

> The json validation API **still works just fine** but we recommend you use the new validation API for new code, and to port your old code whenever it's possible.

## `Reads` migration

The equivalent of a Json `Reads` is a `Rule`. The key difference is that `Reads` assumes Json input, while `Rule` is more generic, and therefore has one more type parameter.

Basically `Reads[String]` == `Rule[JsValue, String]`.

Migrating a Json `Reads` to a `Rule` is just a matter of modifying imports and specifying the input type.

Let's take a typical example from the Json API documentation:

```scala
scala> case class Creature(
     |   name: String,
     |   isDead: Boolean,
     |   weight: Float)
defined class Creature
```

Using the json API, you would have defined something like:

```scala
scala> {
     |   import play.api.libs.json._
     |   import play.api.libs.functional.syntax._
     | 
     |   implicit val creatureReads = (
     |     (__ \ "name").read[String] and
     |     (__ \ "isDead").read[Boolean] and
     |     (__ \ "weight").read[Float]
     |   )(Creature.apply _)
     | 
     |   val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0F)
     |   Json.fromJson[Creature](js)
     | }
res0: play.api.libs.json.JsResult[Creature] = JsSuccess(Creature(gremlins,false,1.0),)
```

Using the new API, this code becomes:

```scala
import jto.validation._
import play.api.libs.json._

implicit val creatureRule = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  ((__ \ "name").read[String] ~
   (__ \ "isDead").read[Boolean] ~
   (__ \ "weight").read[Float]) (Creature.apply _)
}
```
```scala
scala> val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0F)
js: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1}

scala> From[JsValue, Creature](js)
res2: jto.validation.VA[Creature] = Valid(Creature(gremlins,false,1.0))
```

Which appart from the extra imports is very similar. Notice the `From[JsValue]{...}` block, that's one of the nice features of the new validation API. Not only it avoids type repetition, but it also scopes the implicits.

> **Important:** Note that we're importing `Rules._` **inside** the `From[JsValue]{...}` block.
It is recommended to always follow this pattern, as it nicely scopes the implicits, avoiding conflicts and accidental shadowing.

### readNullable

The readNullable method does not exists anymore. Just use a `Rule[JsValue, Option[T]]` instead. `null` and non existing Path will be handled correctly and give you a `None`:

```scala
val nullableStringRule = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "foo").read[Option[String]]
}

val js1 = Json.obj("foo" -> "bar")
val js2 = Json.obj("foo" -> JsNull)
val js3 = Json.obj()
```
```scala
scala> nullableStringRule.validate(js1)
res4: jto.validation.VA[Option[String]] = Valid(Some(bar))

scala> nullableStringRule.validate(js2)
res5: jto.validation.VA[Option[String]] = Valid(None)

scala> nullableStringRule.validate(js3)
res6: jto.validation.VA[Option[String]] = Valid(None)
```

### keepAnd

The general use for `keepAnd` is to apply two validation on the same `JsValue`, for example:

```scala
{
  import play.api.libs.json._
  import Reads._
  import play.api.libs.functional.syntax._
  (JsPath \ "key1").read[String](email keepAnd minLength[String](5))
}
```

You can achieve the same think in the Validation API using [Rules composition](ScalaValidationRule.md)

```scala
From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "key1").read(email |+| minLength(5))
}
```

### lazy reads

Reads are always lazy in the new validation API, therefore you don't need to use any specific function, even for recursive types:

```scala
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class User(id: Long, name: String, friend: Option[User] = None)

implicit lazy val UserReads: Reads[User] = (
  (__ \ 'id).read[Long] and
  (__ \ 'name).read[String] and
  (__ \ 'friend).lazyReadNullable(UserReads)
)(User.apply _)

val js = Json.obj(
  "id" -> 123L,
  "name" -> "bob",
  "friend" -> Json.obj("id" -> 124L, "name" -> "john", "friend" -> JsNull))
```
```scala
scala> Json.fromJson[User](js)
res12: play.api.libs.json.JsResult[User] = JsSuccess(User(123,bob,Some(User(124,john,None))),)
```

becomes:

```scala
case class User(id: Long, name: String, friend: Option[User] = None)

implicit lazy val userRule: Rule[JsValue, User] = From[JsValue]{ __ =>
  import jto.validation.playjson.Rules._
  ((__ \ "id").read[Long] ~
   (__ \ "name").read[String] ~
   (__ \ "friend").read(optionR(userRule))) (User.apply _)
}

val js = Json.obj(
  "id" -> 123L,
  "name" -> "bob",
  "friend" -> Json.obj("id" -> 124L, "name" -> "john", "friend" -> JsNull))
```
```scala
scala> From[JsValue, User](js)
res15: jto.validation.VA[User] = Valid(User(123,bob,Some(User(124,john,None))))
```

### Numeric types

You should be aware that numeric type coercion is a bit stricter in the validation API.

For example:

```scala
scala> val js = Json.obj("n" -> 42.5f)
js: play.api.libs.json.JsObject = {"n":42.5}

scala> js.validate((__ \ "n").read[Int]) // JsSuccess(42, /n)
res16: play.api.libs.json.JsResult[Int] = JsError(List((/n,List(ValidationError(List(error.expected.int),WrappedArray())))))
```

whereas with the validation API, an `Int` must really be an `Int`:

```scala
import json.Rules._
val js = Json.obj("n" -> 42.5f)
(Path \ "n").read[JsValue, Int].validate(js)
```

### `json.apply` and `path.as[T]`

Those methods do not exist in the validation API. Even in the json API, it is generally recommended not to use them as they are "unsafe".

The preferred solution is to use `path.read[T]` and to handle failure properly.

```scala
{
  val js = Json.obj("foo" -> "bar")
  (js \ "foo").as[String]
}
```

becomes

```scala
{
  import jto.validation.playjson.Rules._
  (Path \ "foo").read[JsValue, String]
}
```

### pickBranch

`JsPath` has a `prickBranch` method, that creates a `Reads` extracting a subtree in a Json object:

For example, given the following json object, we can extract a sub tree:

```scala
{
  import play.api.libs.json._

  val js = Json.obj(
    "field1" -> "alpha",
    "field2" -> 123L,
    "field3" -> Json.obj(
      "field31" -> "beta",
      "field32"-> 345
    ))

  val pick = (__ \ "field3").json.pickBranch
  pick.reads(js) // Valid({"field3":{"field31":"beta","field32":345}})
}
```

In the validation API, you simply use `read` to create a rule picking a branch:

```scala
import jto.validation._
import play.api.libs.json._

val js = Json.obj(
  "field1" -> "alpha",
  "field2" -> 123L,
  "field3" -> Json.obj(
    "field31" -> "beta",
    "field32"-> 345
  ))

val pick = From[JsValue] { __ =>
  import jto.validation.playjson.Rules._
  (__ \ "field3").read[JsValue]
}
```
```scala
scala> pick.validate(js)
res22: jto.validation.VA[play.api.libs.json.JsValue] = Valid({"field31":"beta","field32":345})
```

## `Writes` migration

`Writes` are really easy to port. Just like `Reads`, it's basically a matter of adding imports.

For example, you would have defined a `Writes` for the `Creature` case class this way:

```scala
{
  import play.api.libs.json._

  case class Creature(
    name: String,
    isDead: Boolean,
    weight: Float)

  implicit val creatureWrite = (
    (__ \ "name").write[String] and
    (__ \ "isDead").write[Boolean] and
    (__ \ "weight").write[Float]
  ).unlifted(Creature.unapply)

  Json.toJson(Creature("gremlins", false, 1f))
}
```

With the validation API:

```scala
import jto.validation._
import play.api.libs.json._

case class Creature(
  name: String,
  isDead: Boolean,
  weight: Float)

implicit val creatureWrite = To[JsObject]{ __ =>
  import jto.validation.playjson.Writes._
  ((__ \ "name").write[String] ~
   (__ \ "isDead").write[Boolean] ~
   (__ \ "weight").write[Float]).unlifted(Creature.unapply)
}
```
```scala
scala> val c = To[Creature, JsObject](Creature("gremlins", false, 1f))
c: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1}
```

## `Format` migration

The validation API does not have an equivalent for `Format`. We find that generally `Format` is not really convenient since validation and serialization are rarely symmetrical, and you quite often end up havind multiple `Reads` for a given type, making `Format` rather unsettling.

## Json Inception (macro)

Macros are also available for the validation API. See [Validation Inception](ScalaValidationMacros.md).
