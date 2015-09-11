# Extensions: Supporting new types

The validation API is designed to be easily extensible. Supporting new types is just a matter of providing the appropriate set of Rules and Writes.

In this documentation, we'll study the implementation of the Json support. All extensions are to be defined in a similar fashion. The total amount of code needed is rather small, but there's best practices you need to follow.

## Rules

The first step is to define what we call primitive rules. Primitive rules is a set of rules on which you could build any complex validation.

The base of all Rules is the capacity to extract a subset of some input data.

For the type `JsValue`, we need to be able to extract a `JsValue` at a given `Path`:

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json.{KeyPathNode => JSKeyPathNode, IdxPathNode => JIdxPathNode, _}
import play.api.libs.json.{KeyPathNode=>JSKeyPathNode, IdxPathNode=>JIdxPathNode, _}

scala> object Ex1 {
     | 
     | 	def pathToJsPath(p: Path) =
     | 	  play.api.libs.json.JsPath(p.path.map{
     | 	    case KeyPathNode(key) => JSKeyPathNode(key)
     | 	    case IdxPathNode(i) => JIdxPathNode(i)
     | 	  })
     | 
     | 	implicit def pickInJson(p: Path): Rule[JsValue, JsValue] =
     | 		Rule[JsValue, JsValue] { json =>
     | 		  pathToJsPath(p)(json) match {
     | 		    case Nil => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
     | 		    case js :: _ => Valid(js)
     | 		  }
     | 		}
     | }
defined object Ex1
```

Now we are able to do this:

```scala
scala> {
     | 	import Ex1._
     | 
     | 	val js = Json.obj(
     |   "field1" -> "alpha",
     |   "field2" -> 123L,
     |   "field3" -> Json.obj(
     |     "field31" -> "beta",
     |     "field32"-> 345))
     | 
     | 	val pick = From[JsValue]{ __ =>
     | 	  (__ \ "field2").read[JsValue]
     | 	}
     | 
     | 	pick.validate(js)
     | }
res0: jto.validation.VA[play.api.libs.json.JsValue] = Valid(123)
```

Which is nice, but is would be much more convenient if we could extract that value as an `Int`.

One solution is to write the following method:

```scala
implicit def pickIntInJson[O](p: Path): Rule[JsValue, JsValue] = ???
```

But we would end up copying 90% of the code we already wrote.
Instead of doing so, we're going to make `pickInJson` a bit smarter by adding an implicit parameter:

```scala
implicit def pickInJson[O](p: Path)(implicit r: Rule[JsValue, O]): Rule[JsValue, O] =
    Rule[JsValue, JsValue] { json =>
      pathToJsPath(p)(json) match {
        case Nil => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case js :: _ => Valid(js)
      }
    }.compose(r)
```

The now all we have to do is to write a `Rule[JsValue, O]`, and we automatically get the ` Path => Rule[JsValue, O]` we're interested in. The rest is just a matter of defining all the prmitives rules, for example:

```scala
scala> def jsonAs[T](f: PartialFunction[JsValue, Validated[Seq[ValidationError], T]])(args: Any*) =
     | 	Rule.fromMapping[JsValue, T](
     | 	  f.orElse{ case j => Invalid(Seq(ValidationError("validation.invalid", args: _*)))
     | 	})
jsonAs: [T](f: PartialFunction[play.api.libs.json.JsValue,jto.validation.Validated[Seq[jto.validation.ValidationError],T]])(args: Any*)jto.validation.Rule[play.api.libs.json.JsValue,T]

scala> def stringRule = jsonAs[String] {
     | 	case JsString(v) => Valid(v)
     | }("String")
stringRule: jto.validation.Rule[play.api.libs.json.JsValue,String]

scala> def booleanRule = jsonAs[Boolean]{
     | 	case JsBoolean(v) => Valid(v)
     | }("Boolean")
booleanRule: jto.validation.Rule[play.api.libs.json.JsValue,Boolean]
```

The types you generally want to support natively are:

- String
- Boolean
- Int
- Short
- Long
- Float
- Double
- java BigDecimal
- scala BigDecimal

## Higher order Rules

Supporting primitives is nice, but not enough. Users are going to deal with `Seq` and `Option`. We need to support those types too.

### Option

What we want to do is to implement a function that takes a `Path => Rule[JsValue, O]`, an lift it into a `Path => Rule[JsValue, Option[O]]` for any type `O`. The reason we're working on the fully defined `Path => Rule[JsValue, O]` and not just `Rule[JsValue, O]` is because a non existent `Path` must be validated as a `Valid(None)`. If we were to use `pickInJson` on a `Rule[JsValue, Option[O]]`, we would end up with a `Invalid` in the case of non-existing `Path`.

The `play.api.data.mapping.DefaultRules[I]` traits provides a helper for building the desired method. It's signature is:

```scala
protected def opt[J, O](r: => Rule[J, O], noneValues: Rule[I, I]*)(implicit pick: Path => Rule[I, I], coerce: Rule[I, J]): Path = Rule[I, O]
```

- `noneValues` is a List of all the values we should consider to be `None`. For Json that would be `JsNull`.
- `pick` is a extractor. It's going to extract a subtree.
- `coerce` is a type conversion `Rule`
- `r` is a `Rule` to be applied on the data if they are found

All you have to do is to use this method to implement a specialized version for your type.
For example it's defined this way for Json:

```scala
def optionR[J, O](r: => Rule[J, O], noneValues: Rule[JsValue, JsValue]*)(implicit pick: Path => Rule[JsValue, JsValue], coerce: Rule[JsValue, J]): Path => Rule[JsValue, Option[O]]
    = super.opt[J, O](r, (jsNull.map(n => n: JsValue) +: noneValues):_*)
```
Basically it's just the same, but we are now only supporting `JsValue`. We are also adding JsNull is the list of None-ish values.

Despite the type signature funkiness, this function is actually **really** simple to use:

```scala
scala> val maybeEmail = From[JsValue]{ __ =>
     |   import jto.validation.json.Rules._
     |   (__ \ "email").read(optionR(email))
     | }
maybeEmail: jto.validation.Rule[play.api.libs.json.JsValue,Option[String]] = jto.validation.Rule$$anon$3@630dac5e

scala> maybeEmail.validate(Json.obj("email" -> "foo@bar.com"))
res1: jto.validation.VA[Option[String]] = Valid(Some(foo@bar.com))

scala> maybeEmail.validate(Json.obj("email" -> "baam!"))
res2: jto.validation.VA[Option[String]] = Invalid(List((/email,List(ValidationError(List(error.email),WrappedArray())))))

scala> maybeEmail.validate(Json.obj("email" -> JsNull))
res3: jto.validation.VA[Option[String]] = Valid(None)

scala> maybeEmail.validate(Json.obj())
res4: jto.validation.VA[Option[String]] = Valid(None)
```

Alright, so now we can explicitly define rules for optional data.

But what if we write `(__ \ "age").read[Option[Int]]` ? It does not compile !
We need to define an implicit rule for that:

```scala
implicit def option[O](p: Path)(implicit pick: Path => Rule[JsValue, JsValue], coerce: Rule[JsValue, O]): Rule[JsValue, Option[O]] =
    option(Rule.zero[O])(pick, coerce)(p)
```

```scala
scala> val maybeAge = From[JsValue]{ __ =>
     |   import jto.validation.json.Rules._
     |   (__ \ "age").read[Option[Int]]
     | }
maybeAge: jto.validation.Rule[play.api.libs.json.JsValue,Option[Int]] = jto.validation.Rule$$anon$3@11bbedc6
```

### Lazyness

It's very important that every Rule is completely lazily evaluated . The reason for that is that you may be validating recursive types:

```scala
scala> case class RecUser(name: String, friends: Seq[RecUser] = Nil)
defined class RecUser

scala> val u = RecUser(
     |   "bob",
     |   Seq(RecUser("tom")))
u: RecUser = RecUser(bob,List(RecUser(tom,List())))

scala> lazy val w: Rule[JsValue, RecUser] = From[JsValue]{ __ =>
     |   import jto.validation.json.Rules._
     |   ((__ \ "name").read[String] ~
     |    (__ \ "friends").read(seqR(w))) (RecUser.apply _) // !!! recursive rule definition
     | }
w: jto.validation.Rule[play.api.libs.json.JsValue,RecUser] = <lazy>
```

## Writes

Writes are implemented in a similar fashion, but a generally easier to implement. You start by defining a function for writing at a given path:

```scala
scala> {
     | 	implicit def writeJson[I](path: Path)(implicit w: Write[I, JsValue]): Write[I, JsObject] = ???
     | }
```

And you then defines all the primitive writes:

```scala
scala> {
     | 	implicit def anyval[T <: AnyVal] = ???
     | }
```

### Monoid

In order to be able to use writes combinators, you also need to create an implementation of `Monoid` for your output type. For example, to create a complex write of `JsObject`, we had to implement a `Monoid[JsObject]`:

```scala
scala> {
     | 	import cats.Monoid
     |   implicit def jsonMonoid = new Monoid[JsObject] {
     |     def combine(a1: JsObject, a2: JsObject) = a1 deepMerge a2
     |     def empty = Json.obj()
     |   }
     | }
```

from there you're able to create complex writes like:

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> case class User(
     |   name: String,
     |   age: Int,
     |   email: Option[String],
     |   isAlive: Boolean)
defined class User

scala> val userWrite = To[JsObject] { __ =>
     |   import jto.validation.json.Writes._
     |   ((__ \ "name").write[String] ~
     |    (__ \ "age").write[Int] ~
     |    (__ \ "email").write[Option[String]] ~
     |    (__ \ "isAlive").write[Boolean]) (User.unapply _)
     | }
userWrite: jto.validation.Write[User,play.api.libs.json.JsObject] = jto.validation.Write$$anon$3@4e2f4358
```

## Testing

We highly recommend you to test your rules as much as possible. There's a few tricky cases you need to handle properly. You should port the tests in `RulesSpec.scala` and use them on your rules.

> **Next:** - [Cookbook](ScalaValidationCookbook.md)
