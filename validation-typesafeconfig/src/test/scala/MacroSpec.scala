import jto.validation._
import jto.validation.typesafeconfig._, Rules._
import org.scalatest._
import com.typesafe.config._

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
    Rule.gen[ConfigValue, Person]
  }
}

case class Person2(names: List[String])

object Person2 {
  implicit val personRule = {
    Rule.gen[ConfigValue, Person2]
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
      implicit val userReads = Rule.gen[ConfigValue, User]
      userReads.validate(
        ConfigFactory.parseString("""
          name: toto
          age: 45
        """).root
      ) shouldBe (Valid(User(45, "toto")))
    }

    "create a Rule[Dog]" in {
      implicit val userRule = Rule.gen[ConfigValue, User]
      implicit val dogRule = Rule.gen[ConfigValue, Dog]

      dogRule.validate(
          ConfigFactory.parseString("""
            name: medor
            master: {
              name: toto
              age: 45
            }
          """).root
      ) shouldBe (Valid(Dog("medor", User(45, "toto"))))
    }

    "create a Rule[RecUser]" in {

      implicit val catRule = Rule.gen[ConfigValue, Cat]
      catRule.validate(
        ConfigFactory.parseString("""name: minou""").root
      ) shouldBe (Valid(Cat("minou")))

      implicit lazy val recUserRule: Rule[ConfigValue, RecUser] =
        Rule.gen[ConfigValue, RecUser]

      recUserRule.validate(
          ConfigFactory.parseString("""
              name: bob,
              cat: { name: minou },
              hobbies: [bobsleig, manhunting],
              friends: [{
                name: tom
                hobbies: []
                friends: []
              }]
          """).root
      ) shouldBe (Valid(
          RecUser(
              "bob",
              Some(Cat("minou")),
              List("bobsleig", "manhunting"),
              List(RecUser("tom"))
          )
      ))
    }

    "create a Rule[User1]" in {

      implicit lazy val userRule: Rule[ConfigValue, User1] =
        Rule.gen[ConfigValue, User1]
      userRule.validate(
          ConfigFactory.parseString("""
            name:  bob,
            friend:  { name: tom }
          """).root
      ) shouldBe (Valid(
        User1("bob", Some(User1("tom")))
      ))
    }

    "create Rules for classes with overloaded apply method" in {
      implicit val manyAppliesRule = Rule.gen[ConfigValue, ManyApplies]

      manyAppliesRule.validate(
          ConfigFactory.parseString("""
            foo: bob
            bar: 3
          """).root
      ) shouldBe(Valid( ManyApplies("bob", 3) ))
    }

    "create Rules for traits with single apply method" in {
      implicit val notAClassRule = Rule.gen[ConfigValue, NotAClass]

      notAClassRule.validate(
        ConfigFactory.parseString("""x: 3""").root
      ) shouldBe (Valid(AClass(3)))
    }

    "manage Boxed class" in {

      implicit def idRule[A]: Rule[A, Id[A]] =
        Rule.zero[A].map { Id[A](_) }

      implicit def c1Rule[A](implicit rds: Rule[A, Id[A]],
                             e: Path => Rule[ConfigValue, A]) =
        From[ConfigValue] { __ =>
          ((__ \ "id").read(rds) ~ (__ \ "name").read[String])((id, name) =>
                C1[A](id, name))
        }

      val conf = ConfigFactory.parseString("""
        id: 123
        name: toto
      """).root

      c1Rule[Long].validate(conf) shouldBe
      (Valid(C1[Long](Id[Long](123L), "toto")))
    }

    // test to validate it doesn't compile if missing implicit
    /*
    "fail if missing " in {
      implicit val userReads = Rule.gen[ConfigValue, UserFail]
      ()
    }
     */

    "test 21 fields" when {
      "Rule" in {
        implicit val XRule = Rule.gen[ConfigValue, X]
        ()
      }
    }

    "test inception with overriden object" in {
      implicit val programFormat = Rule.gen[ConfigValue, Program]
      ()
    }

    "test case class 1 field" when {
      "Rule" in {
        implicit val totoRule = Rule.gen[ConfigValue, Toto]
        ()
      }
    }

    "test case class 1 field option" when {
      "Rule" in {
        implicit val toto2Rule = Rule.gen[ConfigValue, Toto2]
        ()
      }
    }

    "test case class 1 field list" when {
      "Rule" in {
        implicit val toto3Rule = Rule.gen[ConfigValue, Toto3]
        ()
      }
    }

    "test case class 1 field set" when {
      "Rule" in {
        implicit val toto4Rule = Rule.gen[ConfigValue, Toto4]
        ()
      }
    }

    "test case class 1 field map" when {
      "Rule" in {
        implicit val toto5Rule = Rule.gen[ConfigValue, Toto5]
        ()
      }
    }

    "test case class 1 field seq[Dog]" in {

      implicit val userRule = Rule.gen[ConfigValue, User]
      implicit val dogRule = Rule.gen[ConfigValue, Dog]
      implicit val toto6Rule = Rule.gen[ConfigValue, Toto6]

      val conf = ConfigFactory.parseString("""
        name: [
          {
            name: medor,
            master: {
              name: toto
              age: 45
            }
          },
          {
            name: brutus,
            master: {
              name: tata
              age: 23
            }
          }
        ]
      """).root

      toto6Rule.validate(conf) shouldBe
      (Valid(
              Toto6(Seq(
                      Dog("medor", User(45, "toto")),
                      Dog("brutus", User(23, "tata"))
                  ))
          ))
    }

    "test case reads in companion object" in {
      From[ConfigValue, Person](
        ConfigFactory.parseString("""
          name: bob
          age: 15
        """).root
      ) shouldBe (Valid(Person("bob", 15)))
    }

    "test case single-field in companion object" in {
      From[ConfigValue, Person2](
        ConfigFactory.parseString("""names: [bob, bobby]""").root
      ) shouldBe (Valid(Person2(List("bob", "bobby"))))
    }
  }
}
