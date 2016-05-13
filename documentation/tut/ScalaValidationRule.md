# Validating and transforming data

## Introduction

The API is designed around the concept of `Rule`. A `Rule[I, O]` defines a way to validate and coerce data, from type `I` to type `O`. It's basically a function `I => Validated[O]`, where `I` is the type of the input to validate, and `O` is the expected output type.

## A simple example

Let's say you want to coerce a `String` into an `Float`.
All you need to do is to define a `Rule` from String to Float:

```scala
scala> import jto.validation._
import jto.validation._

scala> def isFloat: Rule[String, Float] = ???
isFloat: jto.validation.Rule[String,Float]
```

When a `String` is parsed into an `Float`, two scenario are possible, either:

- The `String` can be parsed as a `Float`.
- The `String` can NOT be parsed as a `Float`

In a typical Scala application, you would use `Float.parseFloat` to parse a `String`. On an "invalid" value, this method throws a `NumberFormatException`.

When validating data, we'd certainly prefer to avoid exceptions, as the failure case is expected to happen quite often.

Furthermore, your application should handle it properly, for example by sending a nice error message to the end user. The execution flow of the application should not be altered by a parsing failure, but rather be part of the process. Exceptions are definitely not the appropriate tool for the job.

Back, to our `Rule`. For now we'll not implement `isFloat`, actually the validation API comes with a number of built-in Rules, including the `Float` parsing `Rule[String, Float]`.

All you have to do is import the default Rules.

```scala
scala> import jto.validation._
import jto.validation._

scala> object Rules extends GenericRules with ParsingRules
defined object Rules

scala> Rules.floatR
res0: jto.validation.Rule[String,Float] = jto.validation.Rule$$anon$2@145ee456
```

Let's now test it against different String values:

```scala
scala> Rules.floatR.validate("1")
res1: jto.validation.VA[Float] = Valid(1.0)

scala> Rules.floatR.validate("-13.7")
res2: jto.validation.VA[Float] = Valid(-13.7)

scala> Rules.floatR.validate("abc")
res3: jto.validation.VA[Float] = Invalid(List((/,List(ValidationError(List(error.number),WrappedArray(Float))))))
```

> `Rule` is typesafe. You can't apply a `Rule` on an unsupported type, the compiler won't let you:
>
```scala
scala> Rules.floatR
res4: jto.validation.Rule[String,Float] = jto.validation.Rule$$anon$2@2a8ffb8

scala> Rules.floatR.validate(Seq(32))
<console>:20: error: type mismatch;
 found   : Seq[Int]
 required: String
       Rules.floatR.validate(Seq(32))
                                ^
```

"abc" is not a valid `Float` but no exception was thrown. Instead of relying on exceptions, `validate` is returning an object of type `Validated` (here `VA` is just a fancy alias for a special kind of validation).

`Validated` represents possible outcomes of Rule application, it can be either :

- A `Valid`, holding the value being validated
  When we use `Rule.float` on "1", since "1" is a valid representation of a `Float`, it returns `Valid(1.0)`
- A `Invalid`, containing all the errors.
  When we use `Rule.float` on "abc", since "abc" is *not* a valid representation of a `Float`, it returns `Invalid(List((/,List(ValidationError(validation.type-mismatch,WrappedArray(Float))))))`. That `Invalid` tells us all there is to know: it give us a nice message explaining what has failed, and even gives us a parameter `"Float"`, indicating which type the `Rule` expected to find.

> Note that `Validated` is a parameterized type. Just like `Rule`, it keeps track of the input and output types.
The method `validate` of a `Rule[I, O]` always return a `VA[I, O]`

## Defining your own Rules

Creating a new `Rule` is almost as simple as creating a new function.
All there is to do is to pass a function `I => Validated[I, O]` to `Rule.fromMapping`.

This example creates a new `Rule` trying to get the first element of a `List[Int]`.
In case of an empty `List[Int]`, the rule should return a `Invalid`.

```scala
scala> val headInt: Rule[List[Int], Int] = Rule.fromMapping {
     |   case Nil => Invalid(Seq(ValidationError("error.emptyList")))
     |   case head :: _ => Valid(head)
     | }
headInt: jto.validation.Rule[List[Int],Int] = jto.validation.Rule$$anon$2@752efe99
```

```scala
scala> headInt.validate(List(1, 2, 3, 4, 5))
res6: jto.validation.VA[Int] = Valid(1)

scala> headInt.validate(Nil)
res7: jto.validation.VA[Int] = Invalid(List((/,List(ValidationError(List(error.emptyList),WrappedArray())))))
```

We can make this rule a bit more generic:

```scala
scala> def head[T]: Rule[List[T], T] = Rule.fromMapping {
     |   case Nil => Invalid(Seq(ValidationError("error.emptyList")))
     |   case head :: _ => Valid(head)
     | }
head: [T]=> jto.validation.Rule[List[T],T]
```

```scala
scala> head.validate(List('a', 'b', 'c', 'd'))
res8: jto.validation.VA[Char] = Valid(a)

scala> head.validate(List[Char]())
res9: jto.validation.VA[Char] = Invalid(List((/,List(ValidationError(List(error.emptyList),WrappedArray())))))
```

## Composing Rules

Rules composition is very important in this API. `Rule` composition means that, given two `Rule` `a` and `b`, we can easily create a new Rule `c`.

There two different types of composition

### "Sequential" composition

Sequential composition means that given two rules `a: Rule[I, J]` and `b: Rule[J, O]`, we can create a new rule `c: Rule[I, O]`.

Consider the following example: We want to write a `Rule` that given a `List[String]`, takes the first `String` in that `List`, and try to parse it as a `Float`.

We already have defined:

1. `head: Rule[List[T], T]` returns the first element of a `List`
2. `float: Rule[String, Float]` parses a `String` into a `Float`

We've done almost all the work already. We just have to create a new `Rule` the applies the first `Rule` and if it return a `Valid`, apply the second `Rule`.

It would be fairly easy to create such a `Rule` "manually", but we don't have to. A method doing just that is already available:

```scala
scala> val firstFloat: Rule[List[String], Float] = head.andThen(Rules.floatR)
firstFloat: jto.validation.Rule[List[String],Float] = jto.validation.Rule$$anon$2@621533f4

scala> firstFloat.validate(List("1", "2"))
res10: jto.validation.VA[Float] = Valid(1.0)

scala> firstFloat.validate(List("1.2", "foo"))
res11: jto.validation.VA[Float] = Valid(1.2)
```

If the list is empty, we get the error from `head`

```scala
scala> firstFloat.validate(List())
res12: jto.validation.VA[Float] = Invalid(List((/,List(ValidationError(List(error.emptyList),WrappedArray())))))
```

If the first element is not parseable, we get the error from `Rules.float`.

```scala
scala> firstFloat.validate(List("foo", "2"))
res13: jto.validation.VA[Float] = Invalid(List((/,List(ValidationError(List(error.number),WrappedArray(Float))))))
```

Of course everything is still typesafe:

```scala
scala> firstFloat.validate(List(1, 2, 3))
<console>:22: error: type mismatch;
 found   : Int(1)
 required: String
       firstFloat.validate(List(1, 2, 3))
                                ^
<console>:22: error: type mismatch;
 found   : Int(2)
 required: String
       firstFloat.validate(List(1, 2, 3))
                                   ^
<console>:22: error: type mismatch;
 found   : Int(3)
 required: String
       firstFloat.validate(List(1, 2, 3))
                                      ^
```

#### Improving reporting.

All is fine with our new `Rule` but the error reporting when we parse an element is not perfect yet.
When a parsing error happens, the `Invalid` does not tells us that it happened on the first element of the `List`.

To fix that, we can pass  an additionnal parameter to `andThen`:

```scala
scala> val firstFloat2: Rule[List[String],Float] = head.andThen(Path \ 0)(Rules.floatR)
firstFloat2: jto.validation.Rule[List[String],Float] = jto.validation.Rule$$anon$2@7b59d38a

scala> firstFloat2.validate(List("foo", "2"))
res15: jto.validation.VA[Float] = Invalid(List(([0],List(ValidationError(List(error.number),WrappedArray(Float))))))
```

### "Parallel" composition

Parallel composition means that given two rules `a: Rule[I, O]` and `b: Rule[I, O]`, we can create a new rule `c: Rule[I, O]`.

This form of composition is almost exclusively used for the particular case of rules that are purely constraints, that is, a `Rule[I, I]` checking a value of type `I` satisfies a predicate, but does not transform that value.

Consider the following example: We want to write a `Rule` that given a `Int`, check that this `Int` is positive and even.
The validation API already provides `Rules.min`, we have to define `even` ourselves:

```scala
scala> val positive: Rule[Int,Int] = Rules.min(0)
positive: jto.validation.Rule[Int,Int] = jto.validation.Rule$$anon$2@6e83bdcf

scala> val even: Rule[Int,Int] = Rules.validateWith[Int]("error.even"){ _ % 2 == 0 }
even: jto.validation.Rule[Int,Int] = jto.validation.Rule$$anon$2@709bb5a9
```

Now we can compose those rules using `|+|`

```scala
scala> val positiveAndEven: Rule[Int,Int] = positive |+| even
positiveAndEven: jto.validation.Rule[Int,Int] = jto.validation.Rule$$anon$2@64982e85
```

Let's test our new `Rule`:

```scala
scala> positiveAndEven.validate(12)
res16: jto.validation.VA[Int] = Valid(12)

scala> positiveAndEven.validate(-12)
res17: jto.validation.VA[Int] = Invalid(ArrayBuffer((/,List(ValidationError(List(error.min),WrappedArray(0))))))

scala> positiveAndEven.validate(13)
res18: jto.validation.VA[Int] = Invalid(ArrayBuffer((/,List(ValidationError(List(error.even),WrappedArray())))))

scala> positiveAndEven.validate(-13)
res19: jto.validation.VA[Int] = Invalid(ArrayBuffer((/,List(ValidationError(List(error.min),WrappedArray(0)), ValidationError(List(error.even),WrappedArray())))))
```

Note that both rules are applied. If both fail, we get two `ValidationError`.

> **Next:** - [Complex validation with Rule combinators](ScalaValidationRuleCombinators.md)
