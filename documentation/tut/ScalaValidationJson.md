## Validating Json

Take this JSON example:

```json
{
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
}
```

This can be seen as a tree structure using the 2 following structures:

- **JSON object** contains a set of `name` / `value` pairs:
    - `name` is a String
    - `value` can be :
        - string
        - number
        - another JSON object
        - a JSON array
        - true/false
        - null
- **JSON array** is a sequence of values from the previously listed value types.

> If you want to have more info about the exact JSON standard, please go to [json.org](http://json.org/)

## Json Data Types

`play.api.libs.json` package contains 7 JSON data types reflecting exactly the previous structure.
All types inherit from the generic JSON trait, ```JsValue```. As Stated in [the Json API documentation](https://www.playframework.com/documentation/2.3.x/ScalaJson), we can easily parse this String into a JsValue:

```scala
scala> import play.api.libs.json._
import play.api.libs.json._

scala> val json: JsValue = Json.parse("""
     |   {
     |     "user": {
     |       "name" : "toto",
     |       "age" : 25,
     |       "email" : "toto@jmail.com",
     |       "isAlive" : true,
     |       "friend" : {
     |         "name" : "tata",
     |         "age" : 20,
     |         "email" : "tata@coldmail.com"
     |       }
     |     }
     |   }
     |   """)
json: play.api.libs.json.JsValue = {"user":{"name":"toto","age":25,"email":"toto@jmail.com","isAlive":true,"friend":{"name":"tata","age":20,"email":"tata@coldmail.com"}}}
```

This sample is used in all next samples.
The Validation API will work on the JsValue.

## Accessing Path in a JSON tree

The validation API defines a class named `Path`. A `Path` represents a location. Contrarely to `JsPath`, it's not related to any specific type, it's just a location in some data. Most of the time, a `Path` is our entry point into the Validation API.

### Navigating in data using `Path`

```scala
scala> import jto.validation.Path
import jto.validation.Path

scala> val location: Path = Path \ "user" \ "friend"
location: jto.validation.Path = /user/friend
```

`Path` has a `read` method. Just as in the Json API, read will build a `Rule` looking for data of the given type, at that location.
`read` is a paramaterized method it takes two types parameter, `I` and `O`. `I` represent the input we're trying to parse, and `O` is the output type.

For example, `(Path \ "foo").read[JsValue, Int]`, means the we want to parse a value located at path `foo`, in a JsValue, and parse it as an `Int`.

But let's try something much much easier for now:

```scala
scala> import jto.validation.Rule
import jto.validation.Rule

scala> val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
<console>:18: error: No implicit view available from jto.validation.Path => jto.validation.RuleLike[play.api.libs.json.JsValue,play.api.libs.json.JsValue].
       val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
                                                             ^
```

`location.read[JsValue, JsValue]` means the we're trying lookup at `location` in a `JsValue`, and we expect to find a `JsValue` there. Effectivelly, we're just defining a `Rule` that is picking a subtree in a Json.

As you can see, the compiler gives us an error!

The scala compiler is complaining about not finding an implicit Function of type Path => Rule[JsValue, JsValue]. Indeed, unlike the Json API, you have to provide a method to "lookup" into the data you expect to validate. Fortunatelly, such method already exists and is provided for Json. All you have to do is import it:

```scala
scala> import jto.validation.playjson.Rules._
import jto.validation.playjson.Rules._
```

By convention, all usefull validation methods for a given type are to be found in an object called `Rules`. That object contains a bunch of implicits defining how to lookup in the data, and how to coerce some of the possible values of those data into Scala types.

With those implicits in scope, we can finally create our `Rule`.

```scala
scala> val findFriend: Rule[JsValue, JsValue] = location.read[JsValue, JsValue]
findFriend: jto.validation.Rule[play.api.libs.json.JsValue,play.api.libs.json.JsValue] = jto.validation.Rule$$anon$2@50aa2c16
```

Alright, so far we've defined a `Rule` looking for some data of type JsValue, in an object of type JsValue, at `/user/friend`.
Now we need to apply this `Rule` to our data.

```scala
scala> findFriend.validate(json)
res0: jto.validation.VA[play.api.libs.json.JsValue] = Valid({"name":"tata","age":20,"email":"tata@coldmail.com"})
```

When we apply a `Rule`, we have no guarantee whatsoever that it's going to succeed. There's various things that could fail, so instead of just returning some data of type `O`, `validate` returns an instance of `Validated`.
A `Validated` can only have two types: It's either a `Valid` containing the result we expect, or a `Invalid` containing all the errors along with their locations.

Let's try something that we know will fail: We'll try to lookup for a JsValue at a non existing location:

```scala
scala> (Path \ "somenonexistinglocation").read[JsValue, JsValue].validate(json)
res1: jto.validation.VA[play.api.libs.json.JsValue] = Invalid(List((/somenonexistinglocation,List(ValidationError(List(error.required),WrappedArray())))))
```

This time `validate` returns `Invalid`. There's nothing at `somenonexistinglocation` and this failure tells us just that. We required a `JsValue` to be found at that Path, but our requirement was not fullfiled. Note that the `Invalid` does not just contain a `Path` and an error message. It contains a `List[(Path, List[ValidationError])]`. We'll see later that a  single validation could find several errors at a given `Path`, AND find errors at different `Path`
