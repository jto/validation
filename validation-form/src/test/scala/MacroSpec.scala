import jto.validation._
import jto.validation.forms._
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
    Rule.gen[UrlFormEncoded, Person]
  }
  implicit val personWrite = {
    import Writes._
    Write.gen[Person, UrlFormEncoded]
  }
}

case class Person2(names: List[String])

object Person2 {
  implicit val personRule = {
    import Rules._
    Rule.gen[UrlFormEncoded, Person2]
  }
  implicit val personWrite = {
    import Writes._
    Write.gen[Person2, UrlFormEncoded]
  }
}

class MacroSpec extends WordSpec with Matchers {

  "MappingMacros" should {

    "create a Rule[User]" in {
      import Rules._
      implicit val userReads = Rule.gen[UrlFormEncoded, User]
      userReads.validate(Map("name" -> Seq("toto"), "age" -> Seq("45"))) shouldBe (Valid(
              User(45, "toto")))
    }

    "create a Write[User]" in {
      import Writes._
      implicit val userWrites = Write.gen[User, UrlFormEncoded]
      userWrites.writes(User(45, "toto")) shouldBe (Map("name" -> Seq("toto"),
                                                        "age" -> Seq("45")))
    }

    "create a Rule[Dog]" in {
      import Rules._
      implicit val userRule = Rule.gen[UrlFormEncoded, User]
      implicit val dogRule = Rule.gen[UrlFormEncoded, Dog]

      dogRule.validate(
          Map(
              "name" -> Seq("medor"),
              "master.name" -> Seq("toto"),
              "master.age" -> Seq("45")
          )
      ) shouldBe (Valid(Dog("medor", User(45, "toto"))))
    }

    "create a Write[Dog]" in {
      import Writes._
      implicit val userWrite = Write.gen[User, UrlFormEncoded]
      implicit val dogWrite = Write.gen[Dog, UrlFormEncoded]

      dogWrite.writes(Dog("medor", User(45, "toto"))) shouldBe (Map(
              "name" -> Seq("medor"),
              "master.name" -> Seq("toto"),
              "master.age" -> Seq("45")))
    }

    "create a Format[Dog]" in {
      import Rules._
      import Writes._

      implicit val userRule = Format.gen[UrlFormEncoded, UrlFormEncoded, User]
      implicit val dogRule = Format.gen[UrlFormEncoded, UrlFormEncoded, Dog]

      dogRule.validate(
          Map(
              "name" -> Seq("medor"),
              "master.name" -> Seq("toto"),
              "master.age" -> Seq("45")
          )
      ) shouldBe (Valid(Dog("medor", User(45, "toto"))))
    }

    "create a Rule[RecUser]" in {
      import Rules._

      implicit val catRule = Rule.gen[UrlFormEncoded, Cat]

      catRule.validate(
          Map("name" -> Seq("minou"))
      ) shouldBe (Valid(Cat("minou")))

      implicit lazy val recUserRule: Rule[UrlFormEncoded, RecUser] =
        Rule.gen[UrlFormEncoded, RecUser]

      recUserRule.validate(
          Map(
              "name" -> Seq("bob"),
              "cat.name" -> Seq("minou"),
              "hobbies[0]" -> Seq("bobsleig"),
              "hobbies[1]" -> Seq("manhunting"),
              "friends[0].name" -> Seq("tom")
          )
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

      implicit val catWrite = Write.gen[Cat, UrlFormEncoded]
      catWrite.writes(Cat("minou")) shouldBe (Map("name" -> Seq("minou")))

      implicit lazy val recUserWrite: Write[RecUser, UrlFormEncoded] =
        Write.gen[RecUser, UrlFormEncoded]

      recUserWrite.writes(
          RecUser(
              "bob",
              Some(Cat("minou")),
              Seq("bobsleig", "manhunting"),
              Seq(RecUser("tom"))
          )
      ) shouldBe (
          Map("name" -> Seq("bob"),
              "cat.name" -> Seq("minou"),
              "hobbies[0]" -> Seq("bobsleig"),
              "hobbies[1]" -> Seq("manhunting"),
              "friends[0].name" -> Seq("tom"))
      )
    }

    "create a Format[RecUser]" in {
      import Rules._
      import Writes._

      implicit val catFormat = Format.gen[UrlFormEncoded, UrlFormEncoded, Cat]
      val cat = Cat("minou")
      val catMap = Map("name" -> Seq("minou"))

      catFormat.writes(cat) shouldBe (catMap)
      catFormat.validate(catMap) shouldBe (Valid(cat))

      implicit lazy val recUserFormat: Format[UrlFormEncoded,
                                              UrlFormEncoded,
                                              RecUser] =
        Format.gen[UrlFormEncoded, UrlFormEncoded, RecUser]

      val recMap = Map("name" -> Seq("bob"),
                       "cat.name" -> Seq("minou"),
                       "hobbies[0]" -> Seq("bobsleig"),
                       "hobbies[1]" -> Seq("manhunting"),
                       "friends[0].name" -> Seq("tom"))

      val u = RecUser("bob",
                      Some(Cat("minou")),
                      List("bobsleig", "manhunting"),
                      List(RecUser("tom")))

      recUserFormat.validate(recMap) shouldBe (Valid(u))
      recUserFormat.writes(u) shouldBe (recMap)
    }

    "create a Rule[User1]" in {
      import Rules._

      implicit lazy val userRule: Rule[UrlFormEncoded, User1] =
        Rule.gen[UrlFormEncoded, User1]

      userRule.validate(
          Map("name" -> Seq("bob"), "friend.name" -> Seq("tom"))
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
      implicit lazy val userWrites: Write[User1, UrlFormEncoded] =
        Write.gen[User1, UrlFormEncoded]

      userWrites.writes(
          User1("bob", Some(User1("tom")))
      ) shouldBe (Map("name" -> Seq("bob"), "friend.name" -> Seq("tom")))
    }

    "create a Format[User1]" in {
      import Rules._
      import Writes._

      implicit lazy val userFormat: Format[UrlFormEncoded,
                                           UrlFormEncoded,
                                           User1] =
        Format.gen[UrlFormEncoded, UrlFormEncoded, User1]

      val userMap = Map("name" -> Seq("bob"), "friend.name" -> Seq("tom"))
      val user = User1("bob", Some(User1("tom")))

      userFormat.validate(userMap) shouldBe (Valid(user))
      userFormat.writes(user) shouldBe (userMap)
    }

    "manage Boxed class" in {
      import Rules._

      implicit def idRule[A]: Rule[A, Id[A]] =
        Rule.zero[A].map { Id[A](_) }

      implicit def c1Rule[A](implicit rds: Rule[A, Id[A]],
                             e: Path => Rule[UrlFormEncoded, A]) =
        From[UrlFormEncoded] { __ =>
          ((__ \ "id").read(rds) ~
              (__ \ "name").read[String])((id, name) => C1[A](id, name))
        }

      val map = Map("id" -> Seq("123"), "name" -> Seq("toto"))

      c1Rule[Long].validate(map) shouldBe (Valid(
              C1[Long](Id[Long](123L), "toto")))
    }

    /* // test to validate it doesn't compile if missing implicit
    "fail if missing " in {
      import Rules._
      implicit val userReads = Rule.gen[UrlFormEncoded, UserFail]
      ()
    }*/

    "test 21 fields" when {
      "Rule" in {
        import Rules._
        implicit val XRule = Rule.gen[UrlFormEncoded, X]
        ()
      }

      "Write" in {
        import Writes._
        implicit val XWrites = Write.gen[X, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        implicit val XWrites = Format.gen[UrlFormEncoded, UrlFormEncoded, X]
        ()
      }
    }

    "test inception with overriden object" in {
      import Rules._
      implicit val programFormat = Rule.gen[UrlFormEncoded, Program]
      ()
    }

    "test case class 1 field" when {
      "Rule" in {
        import Rules._
        implicit val totoRule = Rule.gen[UrlFormEncoded, Toto]
        ()
      }

      "Write" in {
        import Writes._
        implicit val totoWrite = Write.gen[Toto, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        implicit val totoFormat =
          Format.gen[UrlFormEncoded, UrlFormEncoded, Toto]
        ()
      }
    }

    "test case class 1 field option" when {
      "Rule" in {
        import Rules._
        implicit val toto2Rule = Rule.gen[UrlFormEncoded, Toto2]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto2Write = Write.gen[Toto2, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        implicit val toto2Format =
          Format.gen[UrlFormEncoded, UrlFormEncoded, Toto2]
        ()
      }
    }

    "test case class 1 field list" when {
      "Rule" in {
        import Rules._
        implicit val toto3Rule = Rule.gen[UrlFormEncoded, Toto3]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto3Write = Write.gen[Toto3, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        implicit val toto3Format =
          Format.gen[UrlFormEncoded, UrlFormEncoded, Toto3]
        ()
      }
    }

    "test case class 1 field set" when {
      "Rule" in {
        import Rules._
        implicit val toto4Rule = Rule.gen[UrlFormEncoded, Toto4]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto4Write = Write.gen[Toto4, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        implicit val toto4Format =
          Format.gen[UrlFormEncoded, UrlFormEncoded, Toto4]
        ()
      }
    }

    "test case class 1 field map" when {
      "Rule" in {
        import Rules._
        import Rules.unsafeImplicits._
        implicit val toto5Rule = Rule.gen[UrlFormEncoded, Toto5]
        ()
      }

      "Write" in {
        import Writes._
        implicit val toto5Write = Write.gen[Toto5, UrlFormEncoded]
        ()
      }

      "Format" in {
        import Rules._
        import Writes._
        import Rules.unsafeImplicits._
        implicit val toto5Format =
          Format.gen[UrlFormEncoded, UrlFormEncoded, Toto5]
        ()
      }
    }

    "test case class 1 field seq[Dog]" in {
      import Rules._

      implicit val userRule = Rule.gen[UrlFormEncoded, User]
      implicit val dogRule = Rule.gen[UrlFormEncoded, Dog]
      implicit val toto6Rule = Rule.gen[UrlFormEncoded, Toto6]

      val map = Map("name[0].name" -> Seq("medor"),
                    "name[0].master.name" -> Seq("toto"),
                    "name[0].master.age" -> Seq("45"),
                    "name[1].name" -> Seq("brutus"),
                    "name[1].master.name" -> Seq("tata"),
                    "name[1].master.age" -> Seq("23"))

      toto6Rule.validate(map) shouldBe (Valid(
              Toto6(Seq(
                      Dog("medor", User(45, "toto")),
                      Dog("brutus", User(23, "tata"))
                  ))
          ))
    }

    "test case reads in companion object" in {
      From[UrlFormEncoded, Person](
          To[Person, UrlFormEncoded](Person("bob", 15))
      ) shouldBe (Valid(Person("bob", 15)))
    }

    "test case single-field in companion object" in {
      From[UrlFormEncoded, Person2](
          To[Person2, UrlFormEncoded](Person2(List("bob", "bobby")))
      ) shouldBe (Valid(Person2(List("bob", "bobby"))))
    }
  }
}
