package play.api.data.mapping.json4s

import org.specs2.mutable._
import scala.util.control.Exception._
import play.api.libs.functional._
import play.api.libs.functional.syntax._

import play.api.data.mapping._
import play.api.data.mapping.{ json4s => js }
import js._
import org.json4s._

object MacroSpec extends Specification {

  case class User(age: Int, name: String)
  case class Dog(name: String, master: User)

  case class Cat(name: String)

  case class RecUser(name: String, cat: Option[Cat] = None, hobbies: Seq[String] = Seq(), friends: Seq[RecUser] = Seq())

  case class User1(name: String, friend: Option[User1] = None)

  case class UserMap(name: String, friends: Map[String, UserMap] = Map())

  case class Toto(name: String)
  case class Toto2(name: Option[String])
  case class Toto3(name: List[Double])
  case class Toto4(name: Set[Long])
  case class Toto5(name: Map[String, Int])
  case class Toto6(name: Seq[Dog])
  case class UserFail(name: String, bd: Toto)

  case class Id[A](id: A)
  case class C1[A](id: Id[A], name: String)

  case class X(
    _1: String, _2: String, _3: String, _4: String, _5: String,
    _6: String, _7: String, _8: String, _9: String, _10: String,
    _11: String, _12: String, _13: String, _14: String, _15: String,
    _16: String, _17: String, _18: String, _19: String, _20: String,
    _21: String
  )

  case class Program(id: Long, name: String, logoPath: Option[String], logoThumb: Option[String])
  object Program {
    def programs = List.empty[Program]
  }

  case class Person(name: String, age: Int)
  object Person {
    implicit val personRule = {
      import Rules._
      Rule.gen[JValue, Person]
    }
    implicit val personWrite = {
      import js.Writes._
      Write.gen[Person, JObject]
    }
  }

  case class Person2(names: List[String])

  object Person2{
    implicit val personRule = {
      import js.Rules._
      Rule.gen[JValue, Person2]
    }
    implicit val personWrite = {
      import js.Writes._
      Write.gen[Person2, JObject]
    }
  }

  case class ManyApplies(foo: String, bar: Int)
  object ManyApplies {
    def apply(x: Option[Int]) = 9
    def apply(y: String) = 4
    def apply(x: String, y: String) = 10
  }

  trait NotAClass
  case class AClass(foo: Int) extends NotAClass
  object NotAClass {
    def apply(x: Int): NotAClass = AClass(x)
  }

  "MappingMacros" should {

    "create a Rule[User]" in {
      import Rules._
      import Writes._
      implicit val userReads = Rule.gen[JValue, User]
      userReads.validate(JObject("name" -> JString("toto"), "age" -> JInt(45))) must beEqualTo(Success(User(45, "toto")))
    }

    "create a Write[User]" in {
      import js.Writes._
      implicit val userWrites = Write.gen[User, JObject]
      userWrites.writes(User(45, "toto")) must beEqualTo(JObject("name" -> JString("toto"), "age" -> JInt(45)))
    }

    "create a Rule[Dog]" in {
      import js.Rules._
      implicit val userRule = Rule.gen[JValue, User]
      implicit val dogRule = Rule.gen[JValue, Dog]

      import Writes._
      dogRule.validate(
        JObject(
          "name" -> JString("medor"),
          "master" -> JObject("name" -> JString("toto"), "age" -> JInt(45))
        )
      ) must beEqualTo(Success(Dog("medor", User(45, "toto"))))

    }

    "create a Write[Dog]" in {
      import js.Writes._
      implicit val userWrite = Write.gen[User, JObject]
      implicit val dogWrite = Write.gen[Dog, JObject]

      dogWrite.writes(Dog("medor", User(45, "toto"))) must beEqualTo(
        JObject(
          "name" -> JString("medor"),
          "master" -> JObject("name" -> JString("toto"), "age" -> JInt(45))
        )
      )
    }

    "create a Rule[RecUser]" in {
      import Rules._

      implicit val catRule = Rule.gen[JValue, Cat]
      import Writes._
      catRule.validate(
        JObject("name" -> JString("minou"))
      ) must beEqualTo(Success(Cat("minou")))

      implicit lazy val recUserRule: Rule[JValue, RecUser] =
        Rule.gen[JValue, RecUser]

      recUserRule.validate(
        JObject(
          "name" -> JString("bob"),
          "cat" -> JObject("name" -> JString("minou")),
          "hobbies" -> JArray(List(JString("bobsleig"), JString("manhunting"))),
          "friends" -> JArray(List(JObject( "name" -> JString("tom"), "hobbies" -> JArray(Nil), "friends" -> JArray(Nil))))
        )
      ) must beEqualTo(
        Success(
          RecUser(
            "bob",
            Some(Cat("minou")),
            List("bobsleig", "manhunting"),
            List(RecUser("tom"))
          )
        )
      )

    }

    "create a Write[RecUser]" in {
      import js.Writes._

      implicit val catWrite = Write.gen[Cat, JObject]
      catWrite.writes(Cat("minou")) must beEqualTo(JObject("name" -> JString("minou")))

      implicit lazy val recUserWrite: Write[RecUser, JValue] = Write.gen[RecUser, JObject]

      recUserWrite.writes(
        RecUser(
          "bob",
          Some(Cat("minou")),
          Seq("bobsleig", "manhunting"),
          Seq(RecUser("tom"))
        )
      ) must beEqualTo(
        JObject(
          "name" -> JString("bob"),
          "cat" -> JObject("name" -> JString("minou")),
          "hobbies" -> JArray(List(JString("bobsleig"), JString("manhunting"))),
          "friends" -> JArray(List(JObject( "name" -> JString("tom"), "hobbies" -> JArray(Nil), "friends" -> JArray(Nil))))
        )
      )

    }

    "create a Rule[User1]" in {
      import Rules._

      implicit lazy val userRule: Rule[JValue, User1] = Rule.gen[JValue, User1]
      import Writes._
      userRule.validate(
        JObject(
          "name" -> JString("bob"),
          "friend" -> JObject( "name" -> JString("tom"))
        )
      ) must beEqualTo(
        Success(
          User1(
            "bob",
            Some(User1("tom"))
          )
        )
      )
    }


    "create a writes[User1]" in {
      import js.Writes._
      implicit lazy val userWrites: Write[User1, JValue] = Write.gen[User1, JObject]

      userWrites.writes(
        User1(
          "bob",
          Some(User1("tom")))
      ) must beEqualTo(
        JObject(
          "name" -> JString("bob"),
          "friend" -> JObject( "name" -> JString("tom"))))
    }

    "create Rules for classes with overloaded apply method" in {
      import Rules._
      import js.Writes._
      implicit val manyAppliesRule = Rule.gen[JValue, ManyApplies]

      manyAppliesRule.validate(
        JObject(
          "foo" -> JString("bob"),
          "bar" -> JInt(3))
      ) must beEqualTo(
        Success(
          ManyApplies(
            "bob",
            3
          )
        )
      )
    }

    "create Rules for traits with single apply method" in {
      import Rules._
      import js.Writes._
      implicit val notAClassRule = Rule.gen[JValue, NotAClass]

      notAClassRule.validate(
        JObject(
          "x" -> JInt(3))
      ) must beEqualTo(
        Success(
          AClass(3)
        )
      )
    }

    "manage Boxed class" in {
      import play.api.libs.functional.syntax._
      import Rules._

      implicit def idRule[A]: Rule[A, Id[A]] =
        Rule.zero[A].fmap{ Id[A](_) }

      implicit def c1Rule[A](implicit rds: Rule[A, Id[A]], e: Path => Rule[JValue, A]) =
        From[JValue]{ __ =>
          ((__ \ "id").read(rds) and
           (__ \ "name").read[String])( (id, name) => C1[A](id, name) )
        }

      import Writes._
      val js = JObject(
        "id" -> JInt(123L),
        "name" -> JString("toto"))

      c1Rule[Long].validate(js) must beEqualTo(Success(C1[Long](Id[Long](123L), "toto")))
    }

    // test to validate it doesn't compile if missing implicit
    /*
    "fail if missing " in {
      import Rules._
      implicit val userReads = Rule.gen[JValue, UserFail]
      success
    }
    */

    "test 21 fields" in {
      "Rule" in {
        import Rules._
        implicit val XRule = Rule.gen[JValue, X]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val XWrites = Write.gen[X, JObject]
        success
      }
    }

    "test inception with overriden object" in {
      import Rules._
      implicit val programFormat = Rule.gen[JValue, Program]
      success
    }

    "test case class 1 field" in {
      "Rule" in {
        import Rules._
        implicit val totoRule = Rule.gen[JValue, Toto]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val totoWrite = Write.gen[Toto, JObject]
        success
      }
    }

    "test case class 1 field option" in {
      "Rule" in {
        import Rules._
        implicit val toto2Rule = Rule.gen[JValue, Toto2]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val toto2Write = Write.gen[Toto2, JObject]
        success
      }
    }

    "test case class 1 field list" in {
      "Rule" in {
        import Rules._
        implicit val toto3Rule = Rule.gen[JValue, Toto3]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val toto3Write = Write.gen[Toto3, JObject]
        success
      }
    }

    "test case class 1 field set" in {
      "Rule" in {
        import Rules._
        implicit val toto4Rule = Rule.gen[JValue, Toto4]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val toto4Write = Write.gen[Toto4, JObject]
        success
      }
    }

    "test case class 1 field map" in {
      "Rule" in {
        import Rules._
        implicit val toto5Rule = Rule.gen[JValue, Toto5]
        success
      }

      "Write" in {
        import js.Writes._
        implicit val toto5Write = Write.gen[Toto5, JObject]
        success
      }
    }

    "test case class 1 field seq[Dog]" in {
      import Rules._

      implicit val userRule = Rule.gen[JValue, User]
      implicit val dogRule = Rule.gen[JValue, Dog]
      implicit val toto6Rule = Rule.gen[JValue, Toto6]

      import Writes._

      val js = JObject("name" -> JArray(List(
        JObject(
          "name" -> JString("medor"),
          "master" -> JObject("name" -> JString("toto"), "age" -> JInt(45))
        ),
        JObject(
          "name" -> JString("brutus"),
          "master" -> JObject("name" -> JString("tata"), "age" -> JInt(23))
        )
      )))

      toto6Rule.validate(js) must beEqualTo(Success(
        Toto6(Seq(
          Dog("medor", User(45, "toto")),
          Dog("brutus", User(23, "tata"))
        ))
      ))
    }

    "test case reads in companion object" in {
      From[JValue, Person](
        To[Person, JValue](Person("bob", 15))
      ) must beEqualTo(Success(Person("bob", 15)))
    }

    "test case single-field in companion object" in {
      From[JValue, Person2](
        To[Person2, JValue](Person2(List("bob", "bobby")))
      ) must beEqualTo(Success(Person2(List("bob", "bobby"))))
    }

  }

}