import jto.validation._
import jto.validation.json._
import play.api.libs.json.{JsValue, JsObject, Json}

import org.specs2.mutable._
import org.specs2.execute.Result
import shapeless.test.illTyped

object DerivationSpec extends Specification {
  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fish: Int, friend: Option[Cat]) extends Animal
  case class Pets(dog: Dog, cat: Cat)

  "Derivation syntax should be similar macro syntax" in {
    import Rules._, Writes._

    val derivedRuleJsObject: RuleLike[JsObject, Dog] = Rule.derive
    val macroRuleJsObejct: RuleLike[JsObject, Dog] = Rule.gen[JsObject, Dog]

    val derivedRuleJsValue: RuleLike[JsValue, Dog] = Rule.derive
    val macroRuleJsValue: RuleLike[JsValue, Dog] = Rule.gen[JsValue, Dog]

    val derivedWriteJsObject: WriteLike[Dog, JsObject] = Write.derive
    val macroWriteJsObject: WriteLike[Dog, JsObject] = Write.gen[Dog, JsObject]

    val dogJson = Json.parse("""
      {
        "name": "doge",
        "bones": 0
      }
    """).as[JsObject]
    val dog = Dog("doge", 0)

    derivedRuleJsObject.validate(dogJson) mustEqual macroRuleJsObejct.validate(dogJson)
    derivedRuleJsValue.validate(dogJson) mustEqual macroRuleJsValue.validate(dogJson)
    derivedWriteJsObject.writes(dog) mustEqual macroWriteJsObject.writes(dog)
  }

  "Recursive derivation handles nested case classes" in {
    import Rules._, Writes._

    illTyped("Rule.gen[JsValue, Pets]")
    illTyped("Write.gen[Pets, JsObject]")

    import DerivationRec._

    val pets = Pets(Dog("doge", 0), Cat("miaou", 0, None))
    val petsJson = Json.parse("""
      {
        "dog": {
          "name": "doge",
          "bones": 0
        },
        "cat": {
          "name": "miaou",
          "fish": 0
        }
      }
    """)

    implicitly[WriteLike[Pets, JsObject]].writes(pets) mustEqual petsJson
    implicitly[RuleLike[JsValue, Pets]].validate(petsJson) mustEqual Valid(pets)
  }

  "Recursive derivation handles recursive case classes" in {
    import Rules._, Writes._

    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Write.gen[Cat, JsObject]")

    import DerivationRec._

    val cat = Cat("le chat", 1, Some(Cat("garfield", 0, None)))
    val catJson = Json.parse("""
      {
        "name": "le chat",
        "fish": 1,
        "friend": {
          "name": "garfield",
          "fish": 0
        }
      }
    """)

    implicitly[WriteLike[Cat, JsObject]].writes(cat) mustEqual catJson
    implicitly[RuleLike[JsValue, Cat]].validate(catJson) mustEqual Valid(cat)
  }

  "Non-recursive derivation fails when macro generation fails" in {
    import Rules._, Writes._

    illTyped("Rule.gen[JsValue, Cat]")
    illTyped("Rule.derive: RuleLike[JsValue, Cat]")

    illTyped("Write.gen[Cat, JsObject]")
    illTyped("Write.derive: WriteLike[Cat, JsObject]")

    illTyped("Rule.gen[JsValue, Animal]")
    illTyped("Rule.derive: RuleLike[JsValue, Animal]")

    illTyped("Write.gen[Animal, JsObject]")
    illTyped("Write.derive: WriteLike[Animal, JsObject]")

    true
  }

  "Derivation handles ADTs" in {
    import Rules._, Writes._
    import DerivationRec._

    sealed trait ADT
    case object ADT1 extends ADT
    case object ADT2 extends ADT
    case object ADT3 extends ADT

    illTyped("Write.gen[ADT, JsObject]")
    illTyped("Rule.gen[JsValue, ADT]")

    val a = ADT1
    val aJson = Json.parse("""{"$type":"ADT1"}""")

    implicitly[WriteLike[ADT, JsObject]].writes(a) mustEqual aJson
    implicitly[RuleLike[JsValue, ADT]].validate(aJson) mustEqual Valid(a)
  }

  case class WithOption(os: Option[String])

  "Derivated instance with Option behaves like macro generated instance" in {
    import Rules._, Writes._
    import DerivationRec._

    val woSome = WithOption(Some("ksjdf"))
    val woSomeJson = Json.parse("""{"os": "ksjdf"}""")

    val woNone = WithOption(None)
    val woNoneJson = Json.parse("{}")

    Write.gen[WithOption, JsObject].writes(woSome) mustEqual implicitly[WriteLike[WithOption, JsObject]].writes(woSome)
    Write.gen[WithOption, JsObject].writes(woNone) mustEqual implicitly[WriteLike[WithOption, JsObject]].writes(woNone)

    Rule.gen[JsValue, WithOption].validate(woSomeJson) mustEqual implicitly[RuleLike[JsValue, WithOption]].validate(woSomeJson)
    Rule.gen[JsValue, WithOption].validate(woNoneJson) mustEqual implicitly[RuleLike[JsValue, WithOption]].validate(woNoneJson)
  }

  "Derivation degenerates (but still works!) with minimal imports" in {
    import Rules.{stringR, intR, pickInJson}
    import Writes.{stringW, intW, jsonMonoid, writeJson}
    import DerivationRec._

    case class WithList(s: List[Int])

    val withList = WithList(List(1, 2))
    val withListJson = Json.parse("""
      {
        "s": {
          "$type": "::",
          "head": 1,
          "tl$1": {
            "$type": "::",
            "head": 2,
            "tl$1": {
              "$type": "Nil"
            }
          }
        }
      }
    """)

    implicitly[WriteLike[WithList, JsObject]].writes(withList) mustEqual withListJson
    implicitly[RuleLike[JsValue, WithList]].validate(withListJson) mustEqual Valid(withList)
  }

  "S'il vous plait... derive-moi un mouton !" in {
    import Rules._, Writes._
    import DerivationRec._

    sealed trait A
    case class B(foo: Int) extends A
    case class C(bar: Int) extends A
    case class Cat2(name: String)
    case class Contact2(firstname: String, lastname: String, company: Option[String], informations: Seq[ContactInformation])
    case class ContactInformation(label: String, email: Option[String], phones: Seq[String])
    case class Dog2(name: String, master: User)
    case class Foo(name: String)
    case class Person(name: String, age: Int)
    case class Person2(names: List[String])
    case class Program(id: Long, name: String, logoPath: Option[String], logoThumb: Option[String])
    case class RecUser(name: String, cat: Option[Cat] = None, hobbies: Seq[String] = Seq(), friends: Seq[RecUser] = Seq())
    case class RecUser2(name: String, friends: List[RecUser] = Nil)
    case class RecUser3(name: String, friends: Seq[RecUser] = Nil)
    case class Toto(name: String)
    case class Toto2(name: Option[String])
    case class Toto3(name: List[Double])
    case class Toto4(name: Set[Long])
    case class Toto5(name: Map[String, Int])
    case class Toto6(name: Seq[Dog])
    case class User(age: Int, name: String)
    case class User1(name: String, friend: Option[User1] = None)
    case class User2(id: Long, name: String)
    case class UserFail(name: String, bd: Toto)
    case class UserMap(name: String, friends: Map[String, UserMap] = Map())
    case class WithList(ls: List[String])
    case class X(_1: String, _2: String, _3: String, _4: String, _5: String, _6: String, _7: String, _8: String, _9: String, _10: String, _11: String, _12: String, _13: String, _14: String, _15: String, _16: String, _17: String, _18: String, _19: String, _20: String, _21: String)

    TestRandomly.implicitly[JsValue, JsObject, A]
    TestRandomly.implicitly[JsValue, JsObject, B]
    TestRandomly.implicitly[JsValue, JsObject, C]
    TestRandomly.implicitly[JsValue, JsObject, Cat2]
    TestRandomly.implicitly[JsValue, JsObject, Contact2]
    TestRandomly.implicitly[JsValue, JsObject, ContactInformation]
    TestRandomly.implicitly[JsValue, JsObject, Dog2]
    TestRandomly.implicitly[JsValue, JsObject, Foo]
    TestRandomly.implicitly[JsValue, JsObject, Person2]
    TestRandomly.implicitly[JsValue, JsObject, Program]
    TestRandomly.implicitly[JsValue, JsObject, Person]
    TestRandomly.implicitly[JsValue, JsObject, RecUser]
    TestRandomly.implicitly[JsValue, JsObject, RecUser2]
    TestRandomly.implicitly[JsValue, JsObject, RecUser3]
    TestRandomly.implicitly[JsValue, JsObject, Toto]
    TestRandomly.implicitly[JsValue, JsObject, Toto2]
    TestRandomly.implicitly[JsValue, JsObject, Toto3]
    TestRandomly.implicitly[JsValue, JsObject, Toto4]
    TestRandomly.implicitly[JsValue, JsObject, Toto5]
    TestRandomly.implicitly[JsValue, JsObject, Toto6]
    TestRandomly.implicitly[JsValue, JsObject, User]
    TestRandomly.implicitly[JsValue, JsObject, User1]
    TestRandomly.implicitly[JsValue, JsObject, User2]
    TestRandomly.implicitly[JsValue, JsObject, UserFail]
    TestRandomly.implicitly[JsValue, JsObject, UserMap]
    TestRandomly.implicitly[JsValue, JsObject, WithList]
    TestRandomly.implicitly[JsValue, JsObject, X]
  }
}
