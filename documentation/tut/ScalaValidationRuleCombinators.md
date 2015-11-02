# Combining Rules

## Introduction

We've already explained what a `Rule` is in [the previous chapter](ScalaValidationRule.md).
Those examples were only covering simple rules. However most of the time, rules are used to validate and transform complex hierarchical objects, like [Json](ScalaValidationJson.md), or [Forms](ScalaValidationMigrationForm.md).

The validation API allows complex object rules creation by combining simple rules together. This chapter explains how to create complex rules.

> Despite examples below are validating Json objects, the API is not dedicated only to Json and can be used on any type.
> Please refer to [Validating Json](ScalaValidationJson.md), [Validating Forms](ScalaValidationMigrationForm.md), and [Supporting new types](ScalaValidationExtensions.md) for more information.

## Path

The validation API defines a class named `Path`. A `Path` represents the location of a data among a complex object.
Unlike `JsPath` it is not related to any specific type. It's just a location in some data.
Most of the time, a `Path` is our entry point into the Validation API.

A `Path` is declared using this syntax:

```scala
scala> import jto.validation.Path
import jto.validation.Path

scala> val path = Path \ "foo" \ "bar"
path: jto.validation.Path = /foo/bar
```

`Path` here is the empty `Path` object. One may call it the root path.

A path can also reference indexed data, such as a `Seq`

```scala
scala> val pi = Path \ "foo" \ 0
pi: jto.validation.Path = /foo[0]
```

### Extracting data using `Path`

Consider the following json:

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> val js: JsValue = Json.parse("""{
     |   "user": {
     |     "name" : "toto",
     |     "age" : 25,
     |     "email" : "toto@jmail.com",
     |     "isAlive" : true,
     |     "friend" : {
     |       "name" : "tata",
     |       "age" : 20,
     |       "email" : "tata@coldmail.com"
     |     }
     |   }
     | }""")
js: play.api.libs.json.JsValue = {"user":{"name":"toto","age":25,"email":"toto@jmail.com","isAlive":true,"friend":{"name":"tata","age":20,"email":"tata@coldmail.com"}}}
```

The first step before validating anything is to be able to access a fragment of the complex object.

Assuming you'd like to validate that `friend` exists and is valid in this json, you first need to access the object located at `user.friend` (Javascript notation).

#### The `read` method

We start by creating a `Path` representing the location of the data we're interested in:

```scala
scala> import jto.validation._
import jto.validation._

scala> val location: Path = Path \ "user" \ "friend"
location: jto.validation.Path = /user/friend
```

`Path` has a `read[I, O]` method, where `I` represents the input we're trying to parse, and `O` the output type. For example, `(Path \ "foo").read[JsValue, Int]`, will try to read a value located at path `/foo` in a `JsValue` as an `Int`.

But let's try something much easier for now:

```scala
import jto.validation._
import play.api.libs.json._

val location: Path = Path \ "user" \ "friend"
val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
```

`location.read[JsValue, JsValue]` means we're trying to lookup at `location` in a `JsValue`, and we expect to find a `JsValue` there.
In fact we're defining a `Rule` that is picking a subtree in a `JsValue`.

If you try to run that code, the compiler gives you the following error:

```scala
scala> val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
<console>:25: error: No implicit view available from jto.validation.Path => jto.validation.RuleLike[play.api.libs.json.JsValue,play.api.libs.json.JsValue].
       val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
                                                             ^
```


The Scala compiler is complaining about not finding an implicit function of type `Path => Rule[JsValue, JsValue]`. Indeed, unlike the Json API, you have to provide a method to **lookup** into the data you expect to validate.

Fortunately, such method already exists. All you have to do is to import it:

```scala
scala> import jto.validation.playjson.Rules._
import jto.validation.playjson.Rules._
```

> By convention, all useful validation methods for a given type are to be found in an object called `Rules`. That object contains a bunch of implicits defining how to lookup in the data, and how to coerce some of the possible values of those data into Scala types.

With those implicits in scope, we can finally create our `Rule`:

```scala
scala> val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
findFriend: jto.validation.Rule[play.api.libs.json.JsValue,play.api.libs.json.JsValue] = jto.validation.Rule$$anon$2@6c4ba2c0
```

Alright, so far we've defined a `Rule` looking for some data of type `JsValue`, located at `/user/friend` in an object of type `JsValue`.

Now we need to apply this `Rule` to our data:

```scala
scala> findFriend.validate(js)
res1: jto.validation.VA[play.api.libs.json.JsValue] = Valid({"name":"tata","age":20,"email":"tata@coldmail.com"})
```

If we can't find anything, applying a `Rule` leads to a `Invalid`:

```scala
scala> (Path \ "foobar").read[JsValue, JsValue].validate(js)
res2: jto.validation.VA[play.api.libs.json.JsValue] = Invalid(List((/foobar,List(ValidationError(List(error.required),WrappedArray())))))
```

### Type coercion

We now are capable of extracting data at a given `Path`. Let's do it again on a different sub-tree:

```scala
scala> val age = (Path \ "user" \ "age").read[JsValue, JsValue]
age: jto.validation.Rule[play.api.libs.json.JsValue,play.api.libs.json.JsValue] = jto.validation.Rule$$anon$2@6e26bdfa
```

Let's apply this new `Rule`:

```scala
scala> age.validate(js)
res3: jto.validation.VA[play.api.libs.json.JsValue] = Valid(25)
```

Again, if the json is invalid:

```scala
scala> age.validate(Json.obj())
res4: jto.validation.VA[play.api.libs.json.JsValue] = Invalid(List((/user/age,List(ValidationError(List(error.required),WrappedArray())))))
```

The `Invalid` informs us that it could not find `/user/age` in that `JsValue`.

That example is nice, but we'd certainly prefer to extract `age` as an `Int` rather than a `JsValue`.
All we have to do is to change the output type in our `Rule` definition:

```scala
scala> val age = (Path \ "user" \ "age").read[JsValue, Int]
age: jto.validation.Rule[play.api.libs.json.JsValue,Int] = jto.validation.Rule$$anon$2@6b07d0f0
```

And apply it:

```scala
scala> age.validate(js)
res5: jto.validation.VA[Int] = Valid(25)
```

If we try to parse something that is not an `Int`, we get a `Invalid` with the appropriate Path and error:

```scala
scala> (Path \ "user" \ "name").read[JsValue, Int].validate(js)
res6: jto.validation.VA[Int] = Invalid(List((/user/name,List(ValidationError(List(error.number),WrappedArray(Int))))))
```

So scala *automagically* figures out how to transform a `JsValue` into an `Int`. How does this happens ?

It's fairly simple. The definition of `read` looks like this:

```scala
{
	def read[I, O](implicit r: Path => Rule[I, O]): Rule[I, O] = ???
}
```

So when use `(Path \ "user" \ "age").read[JsValue, Int]`, the compiler looks for an `implicit Path => Rule[JsValue, Int]`, which happens to exist in `play.api.data.mapping.json.Rules`.


### Validated

So far we've managed to lookup for a `JsValue` and transform that `JsValue` into an `Int`. Problem is: not every `Int` is a valid age. An age should always be a positive `Int`.

```scala
scala> val js = Json.parse("""{
     |   "user": {
     |     "age" : -33
     |   }
     | }""")
js: play.api.libs.json.JsValue = {"user":{"age":-33}}

scala> val age = (Path \ "user" \ "age").read[JsValue, Int]
age: jto.validation.Rule[play.api.libs.json.JsValue,Int] = jto.validation.Rule$$anon$2@2afef77b
```

Our current implementation of `age` is rather unsatisfying...

```scala
scala> age.validate(js)
res8: jto.validation.VA[Int] = Valid(-33)
```

We can fix that very simply using `from`, and a built-in `Rule`:

```scala
scala> val positiveAge = (Path \ "user" \ "age").from[JsValue](min(0))
positiveAge: jto.validation.Rule[play.api.libs.json.JsValue,Int] = jto.validation.Rule$$anon$2@5df57bff
```

Let's try that again:

```scala
scala> positiveAge.validate(js)
res9: jto.validation.VA[Int] = Invalid(List((/user/age,List(ValidationError(List(error.min),WrappedArray(0))))))
```

That's better, but still not perfect: 8765 is considered valid:

```scala
scala> val js2 = Json.parse("""{ "user": { "age" : 8765 } }""")
js2: play.api.libs.json.JsValue = {"user":{"age":8765}}

scala> positiveAge.validate(js2)
res10: jto.validation.VA[Int] = Valid(8765)
```

Let's fix our `age` `Rule`:

```scala
scala> val properAge = (Path \ "user" \ "age").from[JsValue](min(0) |+| max(130))
properAge: jto.validation.Rule[play.api.libs.json.JsValue,Int] = jto.validation.Rule$$anon$2@71d8f425
```

and test it:

```scala
scala> val jsBig = Json.parse("""{ "user": { "age" : 8765 } }""")
jsBig: play.api.libs.json.JsValue = {"user":{"age":8765}}

scala> properAge.validate(jsBig)
res11: jto.validation.VA[Int] = Invalid(ArrayBuffer((/user/age,List(ValidationError(List(error.max),WrappedArray(130))))))
```

### Full example

```scala
scala> import jto.validation._
import jto.validation._

scala> import jto.validation.playjson.Rules._
import jto.validation.playjson.Rules._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> val js = Json.parse("""{
     |   "user": {
     |     "name" : "toto",
     |     "age" : 25,
     |     "email" : "toto@jmail.com",
     |     "isAlive" : true,
     |     "friend" : {
     |       "name" : "tata",
     |       "age" : 20,
     |       "email" : "tata@coldmail.com"
     |     }
     |   }
     | }""")
js: play.api.libs.json.JsValue = {"user":{"name":"toto","age":25,"email":"toto@jmail.com","isAlive":true,"friend":{"name":"tata","age":20,"email":"tata@coldmail.com"}}}

scala> val age = (Path \ "user" \ "age").from[JsValue](min(0) |+| max(130))
age: jto.validation.Rule[play.api.libs.json.JsValue,Int] = jto.validation.Rule$$anon$2@3b141b73

scala> age.validate(js)
res12: jto.validation.VA[Int] = Valid(25)
```

## Combining Rules

So far we've validated only fragments of our json object.
Now we'd like to validate the entire object, and turn it into a instance of the `User` class defined below:

```scala
scala> case class User(
     |   name: String,
     |   age: Int,
     |   email: Option[String],
     |   isAlive: Boolean)
defined class User
```

We need to create a `Rule[JsValue, User]`. Creating this Rule is simply a matter of combining together the rules parsing each field of the json.

```scala
scala> import jto.validation._
import jto.validation._

scala> import play.api.libs.json._
import play.api.libs.json._

scala> val userRule = From[JsValue] { __ =>
     |   import jto.validation.playjson.Rules._
     |   ((__ \ "name").read[String] ~
     |    (__ \ "age").read[Int] ~
     |    (__ \ "email").read[Option[String]] ~
     |    (__ \ "isAlive").read[Boolean]) (User.apply)
     | }
userRule: jto.validation.Rule[play.api.libs.json.JsValue,User] = jto.validation.Rule$$anon$3@3e40ed46
```

> **Important:** Note that we're importing `Rules._` **inside** the `From[I]{...}` block.
It is recommended to always follow this pattern, as it nicely scopes the implicits, avoiding conflicts and accidental shadowing.

`From[JsValue]` defines the `I` type of the rules we're combining. We could have written:

```scala
(Path \ "name").read[JsValue, String] ~
(Path \ "age").read[JsValue, Int] ~
//...
```

but repeating `JsValue` all over the place is just not very DRY.

> **Next:** - [Serialization with Write](ScalaValidationWrite.md)
> **For more examples and snippets:** - [Cookbook](ScalaValidationCookbook.md)
