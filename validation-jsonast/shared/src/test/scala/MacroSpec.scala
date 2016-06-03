import jto.validation._
import jto.validation.jsonast._
import org.scalatest._

case class User(age: Int, name: String)
case class Dog(name: String, master: User)
case class Cat(name: String)
case class RecUser(name: String,
                   cat: Option[Cat] = None,
                   hobbies: Seq[String] = Seq(),
                   friends: Seq[RecUser] = Seq())
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
    _1: String,
    _2: String,
    _3: String,
    _4: String,
    _5: String,
    _6: String,
    _7: String,
    _8: String,
    _9: String,
    _10: String,
    _11: String,
    _12: String,
    _13: String,
    _14: String,
    _15: String,
    _16: String,
    _17: String,
    _18: String,
    _19: String,
    _20: String,
    _21: String
)

case class Program(id: Long,
                   name: String,
                   logoPath: Option[String],
                   logoThumb: Option[String])
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
    import Writes._
    Write.gen[Person, JObject]
  }
}

case class Person2(names: List[String])

object Person2 {
  implicit val personRule = {
    import Rules._
    Rule.gen[JValue, Person2]
  }
  implicit val personWrite = {
    import Writes._
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

class MacroSpec extends WordSpec with Matchers {
  "MappingMacros" should {

    "create a Rule[User]" in {
      import Rules._
      implicit val userReads = Rule.gen[JValue, User]
      userReads.validate(JObject(Map("name" -> JString("toto"),
                                     "age" -> JNumber(45)))) shouldBe (Valid(
              User(45, "toto")))
    }

    "create a Write[User]" in {
      import Writes._
      implicit val userWrites = Write.gen[User, JObject]
      userWrites.writes(User(45, "toto")) shouldBe (JObject(
              Map("name" -> JString("toto"), "age" -> JNumber(45))))
    }

    "create a Rule[Dog]" in {
      import Rules._
      implicit val userRule = Rule.gen[JValue, User]
      implicit val dogRule = Rule.gen[JValue, Dog]

      dogRule.validate(
          JObject(Map(
                  "name" -> JString("medor"),
                  "master" -> JObject(
                      Map("name" -> JString("toto"), "age" -> JNumber(45)))
              ))
      ) shouldBe (Valid(Dog("medor", User(45, "toto"))))
    }

    "create a Write[Dog]" in {
      import Writes._
      implicit val userWrite = Write.gen[User, JObject]
      implicit val dogWrite = Write.gen[Dog, JObject]

      dogWrite.writes(Dog("medor", User(45, "toto"))) shouldBe (
          JObject(
              Map(
                  "name" -> JString("medor"),
                  "master" -> JObject(
                      Map("name" -> JString("toto"), "age" -> JNumber(45)))
              ))
      )
    }

    "create a Rule[RecUser]" in {
      import Rules._

      implicit val catRule = Rule.gen[JValue, Cat]
      catRule.validate(
          JObject(Map("name" -> JString("minou")))
      ) shouldBe (Valid(Cat("minou")))

      implicit lazy val recUserRule: Rule[JValue, RecUser] =
        Rule.gen[JValue, RecUser]

      recUserRule.validate(
          JObject(
              Map(
                  "name" -> JString("bob"),
                  "cat" -> JObject(Map("name" -> JString("minou"))),
                  "hobbies" -> JArray(
                      Seq(JString("bobsleig"), JString("manhunting"))),
                  "friends" -> JArray(Seq(JObject(Map("name" -> JString("tom"),
                                                      "hobbies" -> JArray(),
                                                      "friends" -> JArray()))))
              ))
      ) shouldBe (
          Valid(
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
      import Writes._

      implicit val catWrite = Write.gen[Cat, JObject]
      catWrite.writes(Cat("minou")) shouldBe (JObject(
              Map("name" -> JString("minou"))))

      implicit lazy val recUserWrite: Write[RecUser, JValue] =
        Write.gen[RecUser, JObject]

      recUserWrite.writes(
          RecUser(
              "bob",
              Some(Cat("minou")),
              Seq("bobsleig", "manhunting"),
              Seq(RecUser("tom"))
          )
      ) shouldBe (
          JObject(
              Map(
                  "name" -> JString("bob"),
                  "cat" -> JObject(Map("name" -> JString("minou"))),
                  "hobbies" -> JArray(
                      Seq(JString("bobsleig"), JString("manhunting"))),
                  "friends" -> JArray(Seq(JObject(Map("name" -> JString("tom"),
                                                      "hobbies" -> JArray(),
                                                      "friends" -> JArray()))))
              ))
      )
    }

    "create a Rule[User1]" in {
      import Rules._

      implicit lazy val userRule: Rule[JValue, User1] = Rule.gen[JValue, User1]
      userRule.validate(
          JObject(Map(
                  "name" -> JString("bob"),
                  "friend" -> JObject(Map("name" -> JString("tom")))
              ))
      ) shouldBe (
          Valid(
              User1(
                  "bob",
                  Some(User1("tom"))
              )
          )
      )
    }

    "create a writes[User1]" in {
      import Writes._
      implicit lazy val userWrites: Write[User1, JValue] =
        Write.gen[User1, JObject]

      userWrites.writes(
          User1("bob", Some(User1("tom")))
      ) shouldBe (
          JObject(Map("name" -> JString("bob"),
                      "friend" -> JObject(Map("name" -> JString("tom")))))
      )
    }

    "create Rules for classes with overloaded apply method" in {
      import Rules._
      implicit val manyAppliesRule = Rule.gen[JValue, ManyApplies]

      manyAppliesRule.validate(
          JObject(Map("foo" -> JString("bob"), "bar" -> JNumber(3)))
      ) shouldBe (
          Valid(
              ManyApplies(
                  "bob",
                  3
              )
          )
      )
    }

    "create Rules for traits with single apply method" in {
      import Rules._
      implicit val notAClassRule = Rule.gen[JValue, NotAClass]

      notAClassRule.validate(
          JObject(Map("x" -> JNumber(3)))
      ) shouldBe (
          Valid(
              AClass(3)
          )
      )
    }

    "manage Boxed class" in {
      import Rules._

      implicit def idRule[A]: Rule[A, Id[A]] =
        Rule.zero[A].map { Id[A](_) }

      implicit def c1Rule[A](implicit rds: Rule[A, Id[A]],
                             e: Path => Rule[JValue, A]) =
        From[JValue] { __ =>
          ((__ \ "id").read(rds) ~
              (__ \ "name").read[String])((id, name) => C1[A](id, name))
        }

      val js = JObject(Map("id" -> JNumber(123L), "name" -> JString("toto")))

      c1Rule[Long].validate(js) shouldBe (Valid(
              C1[Long](Id[Long](123L), "toto")))
    }

    // test to validate it doesn't compile if missing implicit
    /*
    "fail if missing " in {
      import Rules._
      implicit val userReads = Rule.gen[JValue, UserFail]
      ()
    }
     */

    "test 21 fields" when {
      "Rule" in {
        import Rules._
        implicit val XRule = Rule.gen[JValue, X]
        ()
      }

      "Write" in {
        import Writes._
        implicit val XWrites = Write.gen[X, JObject]
        ()
      }
    }

    "test inception with overriden object" in {
      import Rules._
      implicit val programFormat = Rule.gen[JValue, Program]
      ()
    }

    "test case class 1 field" when {
      "Rule" in {
        import Rules._
        implicit val totoRule = Rule.gen[JValue, Toto]
        ()
      }

      "Write" in {
        import Writes._
        implicit val totoWrite = Write.gen[Toto, JObject]
        ()
      }
    }

    "test case class 1 field option" when {
      "Rule" in {
        import Rules._
        implicit val toto2Rule = Rule.gen[JValue, Toto2]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto2Write = Write.gen[Toto2, JObject]
        ()
      }
    }

    "test case class 1 field list" when {
      "Rule" in {
        import Rules._
        implicit val toto3Rule = Rule.gen[JValue, Toto3]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto3Write = Write.gen[Toto3, JObject]
        ()
      }
    }

    "test case class 1 field set" when {
      "Rule" in {
        import Rules._
        implicit val toto4Rule = Rule.gen[JValue, Toto4]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto4Write = Write.gen[Toto4, JObject]
        ()
      }
    }

    "test case class 1 field map" when {
      "Rule" in {
        import Rules._
        implicit val toto5Rule = Rule.gen[JValue, Toto5]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto5Write = Write.gen[Toto5, JObject]
        ()
      }
    }

    "test case class 1 field seq[Dog]" in {
      import Rules._

      implicit val userRule = Rule.gen[JValue, User]
      implicit val dogRule = Rule.gen[JValue, Dog]
      implicit val toto6Rule = Rule.gen[JValue, Toto6]

      val js = JObject(
          Map("name" -> JArray(
                  Seq(JObject(Map(
                              "name" -> JString("medor"),
                              "master" -> JObject(
                                  Map("name" -> JString("toto"),
                                      "age" -> JNumber(45)))
                          )),
                      JObject(Map("name" -> JString("brutus"),
                                  "master" -> JObject(Map("name" -> JString(
                                                              "tata"),
                                                          "age" -> JNumber(
                                                              23)))))))))

      toto6Rule.validate(js) shouldBe (Valid(
              Toto6(Seq(
                      Dog("medor", User(45, "toto")),
                      Dog("brutus", User(23, "tata"))
                  ))
          ))
    }

    "test case reads in companion object" in {
      From[JValue, Person](
          To[Person, JValue](Person("bob", 15))
      ) shouldBe (Valid(Person("bob", 15)))
    }

    "test case single-field in companion object" in {
      From[JValue, Person2](
          To[Person2, JValue](Person2(List("bob", "bobby")))
      ) shouldBe (Valid(Person2(List("bob", "bobby"))))
    }
  }
}
