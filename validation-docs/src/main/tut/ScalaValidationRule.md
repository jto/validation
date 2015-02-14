# Validating and transforming data

## Introduction

The API is designed around the concept of `Rule`. A `Rule[I, O]` defines a way to validate and coerce data, from type `I` to type `O`. It's basically a function `I => Validation[O]`, where `I` is the type of the input to validate, and `O` is the expected output type.

## A simple example

Let's say you want to coerce a `String` into an `Float`.
All you need to do is to define a `Rule` from String to Float:

```tut
import play.api.data.mapping._
 def isFloat: Rule[String, Float] = ???
 ```

When a `String` is parsed into an `Float`, two scenario are possible, either:

- The `String` can be parsed as a `Float`.
- The `String` can NOT be parsed as a `Float`

In a typical Scala application, you would use `Float.parseFloat` to parse a `String`. On an "invalid" value, this method throws a `NumberFormatException`.

When validating data, we'd certainly prefer to avoid exceptions, as the failure case is expected to happen quite often.

Furthermore, your application should handle it properly, for example by sending a nice error message to the end user. The execution flow of the application should not be altered by a parsing failure, but rather be part of the process. Exceptions are definitely not the appropriate tool for the job.

Back, to our `Rule`. For now we'll not implement `isFloat`, actually the validation API comes with a number of built-in Rules, including the `Float` parsing `Rule[String, Float]`.

All you have to do is import the default Rules.

```tut
import play.api.data.mapping._
object Rules extends GenericRules with ParsingRules
Rules.floatR
```

Let's now test it against different String values:

```tut
Rules.floatR.validate("1")
Rules.floatR.validate("-13.7")
Rules.floatR.validate("abc")
```

> `Rule` is typesafe. You can't apply a `Rule` on an unsupported type, the compiler won't let you:
>
```tut:nofail
Rules.floatR
Rules.floatR.validate(Seq(32))
```

"abc" is not a valid `Float` but no exception was thrown. Instead of relying on exceptions, `validate` is returning an object of type `Validation` (here `VA` is just a fancy alias for a special kind of validation).

`Validation` represents possible outcomes of Rule application, it can be either :

- A `Success`, holding the value being validated
  When we use `Rule.float` on "1", since "1" is a valid representation of a `Float`, it returns `Success(1.0)`
- A `Failure`, containing all the errors.
  When we use `Rule.float` on "abc", since "abc" is *not* a valid representation of a `Float`, it returns `Failure(List((/,List(ValidationError(validation.type-mismatch,WrappedArray(Float))))))`. That `Failure` tells us all there is to know: it give us a nice message explaining what has failed, and even gives us a parameter `"Float"`, indicating which type the `Rule` expected to find.

> Note that `Validation` is a parameterized type. Just like `Rule`, it keeps track of the input and output types.
The method `validate` of a `Rule[I, O]` always return a `VA[I, O]`

## Defining your own Rules

Creating a new `Rule` is almost as simple as creating a new function.
All there is to do is to pass a function `I => Validation[I, O]` to `Rule.fromMapping`.

This example creates a new `Rule` trying to get the first element of a `List[Int]`.
In case of an empty `List[Int]`, the rule should return a `Failure`.

```tut
val headInt: Rule[List[Int], Int] = Rule.fromMapping {
  case Nil => Failure(Seq(ValidationError("error.emptyList")))
  case head :: _ => Success(head)
}
```

```tut
headInt.validate(List(1, 2, 3, 4, 5))
headInt.validate(Nil)
```

We can make this rule a bit more generic:

```tut
def head[T]: Rule[List[T], T] = Rule.fromMapping {
  case Nil => Failure(Seq(ValidationError("error.emptyList")))
  case head :: _ => Success(head)
}
```

```tut
head.validate(List('a', 'b', 'c', 'd'))
head.validate(List[Char]())
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

We've done almost all the work already. We just have to create a new `Rule` the applies the first `Rule` and if it return a `Success`, apply the second `Rule`.

It would be fairly easy to create such a `Rule` "manually", but we don't have to. A method doing just that is already available:

```tut
val firstFloat: Rule[List[String], Float] = head.compose(Rules.floatR)
firstFloat.validate(List("1", "2"))
firstFloat.validate(List("1.2", "foo"))
```

If the list is empty, we get the error from `head`

```tut
firstFloat.validate(List())
```

If the first element is not parseable, we get the error from `Rules.float`.

```tut
firstFloat.validate(List("foo", "2"))
```

Of course everything is still typesafe:

```tut:nofail
firstFloat.validate(List(1, 2, 3))
```

#### Improving reporting.

All is fine with our new `Rule` but the error reporting when we parse an element is not perfect yet.
When a parsing error happens, the `Failure` does not tells us that it happened on the first element of the `List`.

To fix that, we can pass  an additionnal parameter to `compose`:

```tut
val firstFloat2: Rule[List[String],Float] = head.compose(Path \ 0)(Rules.floatR)
firstFloat2.validate(List("foo", "2"))
```

### "Parallel" composition

Parallel composition means that given two rules `a: Rule[I, O]` and `b: Rule[I, O]`, we can create a new rule `c: Rule[I, O]`.

This form of composition is almost exclusively used for the particular case of rules that are purely constraints, that is, a `Rule[I, I]` checking a value of type `I` satisfies a predicate, but does not transform that value.

Consider the following example: We want to write a `Rule` that given a `Int`, check that this `Int` is positive and even.
The validation API already provides `Rules.min`, we have to define `even` ourselves:

```tut
val positive: Rule[Int,Int] = Rules.min(0)
val even: Rule[Int,Int] = Rules.validateWith[Int]("error.even"){ _ % 2 == 0 }
```

Now we can compose those rules using `|+|`

```tut
val positiveAndEven: Rule[Int,Int] = positive |+| even
```

Let's test our new `Rule`:

```tut
positiveAndEven.validate(12)
positiveAndEven.validate(-12)
positiveAndEven.validate(13)
positiveAndEven.validate(-13)
```

Note that both rules are applied. If both fail, we get two `ValidationError`.

> **Next:** - [[Complex validation with Rule combinators | ScalaValidationRuleCombinators]]
