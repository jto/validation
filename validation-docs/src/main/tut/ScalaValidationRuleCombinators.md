# Combining Rules

## Introduction

We've already explained what a `Rule` is in [the previous chapter](ScalaValidationRule.md).
Those examples were only covering simple rules. However most of the time, rules are used to validate and transform complex hierarchical objects, like [Json](ScalaValidationJson.md), or [Forms](ScalaValidationForm.md).

The validation API allows complex object rules creation by combining simple rules together. This chapter explains how to create complex rules.

> Despite examples below are validating Json objects, the API is not dedicated only to Json and can be used on any type.
> Please refer to [Validating Json](ScalaValidationJson.md), [Validating Forms](ScalaValidationForm.md), and [Supporting new types](ScalaValidationExtensions.md) for more information.

## Path

The validation API defines a class named `Path`. A `Path` represents the location of a data among a complex object.
Unlike `JsPath` it is not related to any specific type. It's just a location in some data.
Most of the time, a `Path` is our entry point into the Validation API.

A `Path` is declared using this syntax:

```tut
import play.api.data.mapping.Path
val path = Path \ "foo" \ "bar"
```

`Path` here is the empty `Path` object. One may call it the root path.

A path can also reference indexed data, such as a `Seq`

```tut
val pi = Path \ "foo" \ 0
```

### Extracting data using `Path`

Consider the following json:

```tut
import play.api.libs.json._

val js: JsValue = Json.parse("""{
  "user": {
    "name" : "toto",
    "age" : 25,
    "email" : "toto@jmail.com",
    "isAlive" : true,
    "friend" : {
      "name" : "tata",
      "age" : 20,
      "email" : "tata@coldmail.com"
    }
  }
}""")
```

The first step before validating anything is to be able to access a fragment of the complex object.

Assuming you'd like to validate that `friend` exists and is valid in this json, you first need to access the object located at `user.friend` (Javascript notation).

#### The `read` method

We start by creating a `Path` representing the location of the data we're interested in:

```tut
import play.api.data.mapping._
val location: Path = Path \ "user" \ "friend"
```

`Path` has a `read[I, O]` method, where `I` represents the input we're trying to parse, and `O` the output type. For example, `(Path \ "foo").read[JsValue, Int]`, will try to read a value located at path `/foo` in a `JsValue` as an `Int`.

But let's try something much easier for now:

```tut:silent:nofail
import play.api.libs.json.JsValue
import play.api.data.mapping._

val location: Path = Path \ "user" \ "friend"
val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
```

`location.read[JsValue, JsValue]` means we're trying to lookup at `location` in a `JsValue`, and we expect to find a `JsValue` there.
In fact we're defining a `Rule` that is picking a subtree in a `JsValue`.

If you try to run that code, the compiler gives you the following error:

```tut:nofail
val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
```


The Scala compiler is complaining about not finding an implicit function of type `Path => Rule[JsValue, JsValue]`. Indeed, unlike the Json API, you have to provide a method to **lookup** into the data you expect to validate.

Fortunately, such method already exists. All you have to do is to import it:

```tut
import play.api.data.mapping.json.Rules._
```

> By convention, all useful validation methods for a given type are to be found in an object called `Rules`. That object contains a bunch of implicits defining how to lookup in the data, and how to coerce some of the possible values of those data into Scala types.

With those implicits in scope, we can finally create our `Rule`:

```tut
val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
```

Alright, so far we've defined a `Rule` looking for some data of type `JsValue`, located at `/user/friend` in an object of type `JsValue`.

Now we need to apply this `Rule` to our data:

```tut
findFriend.validate(js)
```

If we can't find anything, applying a `Rule` leads to a `Failure`:

```tut
(Path \ "foobar").read[JsValue, JsValue].validate(js)
```

### Type coercion

We now are capable of extracting data at a given `Path`. Let's do it again on a different sub-tree:

```tut
val age = (Path \ "user" \ "age").read[JsValue, JsValue]
```

Let's apply this new `Rule`:

```tut
age.validate(js)
```

Again, if the json is invalid:

```tut
age.validate(Json.obj())
```

The `Failure` informs us that it could not find `/user/age` in that `JsValue`.

That example is nice, but we'd certainly prefer to extract `age` as an `Int` rather than a `JsValue`.
All we have to do is to change the output type in our `Rule` definition:

```tut
val age = (Path \ "user" \ "age").read[JsValue, Int]
```

And apply it:

```tut
age.validate(js)
```

If we try to parse something that is not an `Int`, we get a `Failure` with the appropriate Path and error:

```tut
(Path \ "user" \ "name").read[JsValue, Int].validate(js)
```

So scala *automagically* figures out how to transform a `JsValue` into an `Int`. How does this happens ?

It's fairly simple. The definition of `read` looks like this:

```tut:silent
{
	def read[I, O](implicit r: Path => Rule[I, O]): Rule[I, O] = ???
}
```

So when use `(Path \ "user" \ "age").read[JsValue, Int]`, the compiler looks for an `implicit Path => Rule[JsValue, Int]`, which happens to exist in `play.api.data.mapping.json.Rules`.


### Validation

So far we've managed to lookup for a `JsValue` and transform that `JsValue` into an `Int`. Problem is: not every `Int` is a valid age. An age should always be a positive `Int`.

```tut
val js = Json.parse("""{
  "user": {
    "age" : -33
  }
}""")

val age = (Path \ "user" \ "age").read[JsValue, Int]
```

Our current implementation of `age` is rather unsatisfying...

```tut
age.validate(js)
```

We can fix that very simply using `from`, and a built-in `Rule`:

```tut
val positiveAge = (Path \ "user" \ "age").from[JsValue](min(0))
```

Let's try that again:

```tut
positiveAge.validate(js)
```

That's better, but still not perfect: 8765 is considered valid:

```tut
val js2 = Json.parse("""{ "user": { "age" : 8765 } }""")
positiveAge.validate(js2)
```

Let's fix our `age` `Rule`:

```tut
val properAge = (Path \ "user" \ "age").from[JsValue](min(0) |+| max(130))
```

and test it:

```tut
val jsBig = Json.parse("""{ "user": { "age" : 8765 } }""")
properAge.validate(jsBig)
```

### Full example

```tut
import play.api.libs.json.Json
import play.api.data.mapping._
import play.api.data.mapping.json.Rules._

val js = Json.parse("""{
  "user": {
    "name" : "toto",
    "age" : 25,
    "email" : "toto@jmail.com",
    "isAlive" : true,
    "friend" : {
      "name" : "tata",
      "age" : 20,
      "email" : "tata@coldmail.com"
    }
  }
}""")

val age = (Path \ "user" \ "age").from[JsValue](min(0) |+| max(130))
age.validate(js)
```

## Combining Rules

So far we've validated only fragments of our json object.
Now we'd like to validate the entire object, and turn it into a instance of the `User` class defined below:

```tut
case class User(
  name: String,
  age: Int,
  email: Option[String],
  isAlive: Boolean)
```

We need to create a `Rule[JsValue, User]`. Creating this Rule is simply a matter of combining together the rules parsing each field of the json.

```tut
import play.api.libs.json._
import play.api.data.mapping._

val userRule = From[JsValue] { __ =>
  import play.api.data.mapping.json.Rules._
  ((__ \ "name").read[String] and
   (__ \ "age").read[Int] and
   (__ \ "email").read[Option[String]] and
   (__ \ "isAlive").read[Boolean])(User.apply _)
}
```

> **Important:** Note that we're importing `Rules._` **inside** the `From[I]{...}` block.
It is recommended to always follow this pattern, as it nicely scopes the implicits, avoiding conflicts and accidental shadowing.

`From[JsValue]` defines the `I` type of the rules we're combining. We could have written:

```scala
 (Path \ "name").read[JsValue, String] and
 (Path \ "age").read[JsValue, Int] and
 //...
```

but repeating `JsValue` all over the place is just not very DRY.

> **Next:** - [Serialization with Write](ScalaValidationWrite.md)
> **For more examples and snippets:** - [Cookbook](ScalaValidationCookbook.md)
