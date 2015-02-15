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
     | 	import play.api.libs.json._
     | 	import play.api.libs.functional.syntax._
     | 	implicit val creatureReads = (
     | 	  (__ \ "name").read[String] ~
     | 	  (__ \ "isDead").read[Boolean] ~
     | 	  (__ \ "weight").read[Float]
     | 	)(Creature.apply _)
     | 	val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0F)
     | 	Json.fromJson[Creature](js)
     | }
res0: play.api.libs.json.JsResult[Creature] = JsSuccess(Creature(gremlins,false,1.0),)
```

Using the new API, this code becomes:

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> import play.api.data.mapping._
import play.api.data.mapping._

scala> implicit val creatureRule = From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules._
     |   (
     |     (__ \ "name").read[String] ~
     |     (__ \ "isDead").read[Boolean] ~
     |     (__ \ "weight").read[Float]
     |   )(Creature.apply _)
     | }
creatureRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,Creature] = play.api.data.mapping.Rule$$anon$2@28aad66e

scala> val js = Json.obj( "name" -> "gremlins", "isDead" -> false, "weight" -> 1.0F)
js: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1.0}

scala> From[JsValue, Creature](js)
res1: play.api.data.mapping.VA[Creature] = Success(Creature(gremlins,false,1.0))
```

Which appart from the extra imports is very similar. Notice the `From[JsValue]{...}` block, that's one of the nice features of the new validation API. Not only it avoids type repetition, but it also scopes the implicits.

> **Important:** Note that we're importing `Rules._` **inside** the `From[JsValue]{...}` block.
It is recommended to always follow this pattern, as it nicely scopes the implicits, avoiding conflicts and accidental shadowing.

### readNullable

The readNullable method does not exists anymore. Just use a `Rule[JsValue, Option[T]]` instead. `null` and non existing Path will be handled correctly and give you a `None`:

```scala
scala> val nullableStringRule = From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules._
     |   (__ \ "foo").read[Option[String]]
     | }
nullableStringRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,Option[String]] = play.api.data.mapping.Rule$$anon$2@4dcd5989

scala> val js1 = Json.obj("foo" -> "bar")
js1: play.api.libs.json.JsObject = {"foo":"bar"}

scala> val js2 = Json.obj("foo" -> JsNull)
js2: play.api.libs.json.JsObject = {"foo":null}

scala> val js3 = Json.obj()
js3: play.api.libs.json.JsObject = {}

scala> nullableStringRule.validate(js1)
res2: play.api.data.mapping.VA[Option[String]] = Success(Some(bar))

scala> nullableStringRule.validate(js2)
res3: play.api.data.mapping.VA[Option[String]] = Success(None)

scala> nullableStringRule.validate(js3)
res4: play.api.data.mapping.VA[Option[String]] = Success(None)
```

### keepAnd

The general use for `keepAnd` is to apply two validation on the same `JsValue`, for example:

```scala
scala> {
     | 	import play.api.libs.json._
     |   import Reads._
     |   import play.api.libs.functional.syntax._
     | 	(JsPath \ "key1").read[String](email keepAnd minLength[String](5))
     | }
res5: play.api.libs.json.Reads[String] = play.api.libs.json.Reads$$anon$8@6bd9f147
```

You can achieve the same think in the Validation API using [[Rules composition|ScalaValidationRule]]

```scala
scala> From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules._
     |   (__ \ "key1").read(email |+| minLength(5))
     | }
res6: play.api.data.mapping.Rule[play.api.libs.json.JsValue,String] = play.api.data.mapping.Rule$$anon$2@53e57a41
```

### lazy reads

Reads are always lazy in the new validation API, therefore you don't need to use any specific function, even for recursive types:

```scala
scala> {
     | 	import play.api.libs.json._
     |   import play.api.libs.functional.syntax._
     |   case class User(id: Long, name: String, friend: Option[User] = None)
     |   implicit lazy val UserReads: Reads[User] = (
     |     (__ \ 'id).read[Long] and
     |     (__ \ 'name).read[String] and
     |     (__ \ 'friend).lazyReadNullable(UserReads)
     |   )(User.apply _)
     |   val js = Json.obj(
     |     "id" -> 123L,
     |     "name" -> "bob",
     |     "friend" -> Json.obj("id" -> 124L, "name" -> "john", "friend" -> JsNull))
     |   Json.fromJson[User](js)
     | }
warning: there were 1 feature warning(s); re-run with -feature for details
res7: play.api.libs.json.JsResult[User] forSome { type User <: Product with Serializable{val id: Long; val name: String; val friend: Option[User]; def copy(id: Long,name: String,friend: Option[User]): User; def copy$default$1: Long @scala.annotation.unchecked.uncheckedVariance; def copy$default$2: String @scala.annotation.unchecked.uncheckedVariance; def copy$default$3: Option[User] @scala.annotation.unchecked.uncheckedVariance} } = JsSuccess(User(123,bob,Some(User(124,john,None))),)
```

becomes:

```scala
scala> case class User(id: Long, name: String, friend: Option[User] = None)
defined class User

scala> implicit lazy val userRule: Rule[JsValue, User] = From[JsValue]{ __ =>
     |   import play.api.data.mapping.json.Rules._
     |   (
     |     (__ \ "id").read[Long] and
     |     (__ \ "name").read[String] and
     |     (__ \ "friend").read(optionR(userRule))
     |   )(User.apply _)
     | }
userRule: play.api.data.mapping.Rule[play.api.libs.json.JsValue,User] = <lazy>

scala> val js = Json.obj(
     |   "id" -> 123L,
     |   "name" -> "bob",
     |   "friend" -> Json.obj("id" -> 124L, "name" -> "john", "friend" -> JsNull))
js: play.api.libs.json.JsObject = {"id":123,"name":"bob","friend":{"id":124,"name":"john","friend":null}}

scala> From[JsValue, User](js)
res8: play.api.data.mapping.VA[User] = Success(User(123,bob,Some(User(124,john,None))))
```

### Numeric types

You should be aware that numeric type coercion is a bit stricter in the validation API.

For example:

```scala
scala> val js = Json.obj("n" -> 42.5f)
js: play.api.libs.json.JsObject = {"n":42.5}

scala> js.validate((__ \ "n").read[Int]) // JsSuccess(42, /n)
res9: play.api.libs.json.JsResult[Int] = JsSuccess(42,/n)
```

whereas with the validation API, an `Int` must really be an `Int`:

```scala
scala> import json.Rules._
import json.Rules._

scala> val js = Json.obj("n" -> 42.5f)
js: play.api.libs.json.JsObject = {"n":42.5}

scala> (Path \ "n").read[JsValue, Int].validate(js)
res10: play.api.data.mapping.VA[Int] = Failure(List((/n,List(ValidationError(error.number,WrappedArray(Int))))))
```

### `json.apply` and `path.as[T]`

Those methods do not exist in the validation API. Even in the json API, it is generally recommended not to use them as they are "unsafe".

The preferred solution is to use `path.read[T]` and to handle failure properly.

```scala
scala> {
     | 	val js = Json.obj("foo" -> "bar")
     | 	(js \ "foo").as[String]
     | }
res11: String = bar
```

becomes

```scala
scala> (Path \ "foo").read[JsValue, String]
res12: play.api.data.mapping.Rule[play.api.libs.json.JsValue,String] = play.api.data.mapping.Rule$$anon$1@4b0fe660
```

### pickBranch

`JsPath` has a `prickBranch` method, that creates a `Reads` extracting a subtree in a Json object:

For example, given the following json object, we can extract a sub tree:

```scala
scala> {
     | 	import play.api.libs.json._
     | 	val js = Json.obj(
     | 		"field1" -> "alpha",
     | 		"field2" -> 123L,
     | 		"field3" -> Json.obj(
     | 		  "field31" -> "beta",
     | 		  "field32"-> 345
     | 		))
     | 	val pick = (__ \ "field3").json.pickBranch
     | 	pick.reads(js) // Success({"field3":{"field31":"beta","field32":345}})
     | }
res13: play.api.libs.json.JsResult[play.api.libs.json.JsObject] = JsSuccess({"field3":{"field31":"beta","field32":345}},/field3)
```

In the validation API, you simply use `read` to create a rule picking a branch:

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> import play.api.data.mapping._
import play.api.data.mapping._

scala> val js = Json.obj(
     | 	"field1" -> "alpha",
     | 	"field2" -> 123L,
     | 	"field3" -> Json.obj(
     | 	  "field31" -> "beta",
     | 	  "field32"-> 345
     | 	))
js: play.api.libs.json.JsObject = {"field1":"alpha","field2":123,"field3":{"field31":"beta","field32":345}}

scala> val pick = From[JsValue]{ __ =>
     | 	import play.api.data.mapping.json.Rules._
     | 	(__ \ "field3").read[JsValue]
     | }
pick: play.api.data.mapping.Rule[play.api.libs.json.JsValue,play.api.libs.json.JsValue] = play.api.data.mapping.Rule$$anon$2@7a244b54

scala> pick.validate(js) // Success({"field31":"beta","field32":345})
res14: play.api.data.mapping.VA[play.api.libs.json.JsValue] = Success({"field31":"beta","field32":345})
```

## `Writes` migration

`Writes` are really easy to port. Just like `Reads`, it's basically a matter of adding imports.

For example, you would have defined a `Writes` for the `Creature` case class this way:

```scala
scala> {
     | 	import play.api.libs.json._
     | 	import play.api.libs.functional.syntax._
     | 	case class Creature(
     | 	  name: String,
     | 	  isDead: Boolean,
     | 	  weight: Float)
     | 	implicit val creatureWrite =
     | 		(
     | 		  (__ \ "name").write[String] ~
     | 		  (__ \ "isDead").write[Boolean] ~
     | 		  (__ \ "weight").write[Float]
     | 		)(unlift(Creature.unapply _))
     | 	Json.toJson(Creature("gremlins", false, 1f))
     | }
res15: play.api.libs.json.JsValue = {"name":"gremlins","isDead":false,"weight":1.0}
```

With the validation API:

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
     | 	import play.api.data.mapping.json.Writes._
     | 	(
     | 	  (__ \ "name").write[String] ~
     | 	  (__ \ "isDead").write[Boolean] ~
     | 	  (__ \ "weight").write[Float]
     | 	)(unlift(Creature.unapply _))
     | }
creatureWrite: play.api.data.mapping.Write[Creature,play.api.libs.json.JsObject] = play.api.data.mapping.Write$$anon$2@7ff930a8

scala> val c = To[Creature, JsObject](Creature("gremlins", false, 1f))
c: play.api.libs.json.JsObject = {"name":"gremlins","isDead":false,"weight":1.0}
```

## `Format` migration

The validation API does not have an equivalent for `Format`. We find that generally `Format` is not really convenient since validation and serialization are rarely symmetrical, and you quite often end up havind multiple `Reads` for a given type, making `Format` rather unsettling.

## Json Inception (macro)

Macros are also available for the validation API. See [[Validation Inception|ScalaValidationMacros]].

> **Next:** - [[Migration from 2.1.x Form API|ScalaValidationMigrationForm]]
