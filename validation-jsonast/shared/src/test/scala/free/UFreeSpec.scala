package jto.validation.free

import jto.validation._
import jto.validation.jsonast._
import jto.validation.jsonast.Rules._
import jto.validation.jsonast.Writes._
import org.scalatest._
import Instances._

case class MyCC1(int: Int, double: Double)
case class MyCC2(mycc: MyCC1, boolean: Boolean)
case class User(age: Int, name: String)
case class Dog(name: String, master: User)
case class Cat(name: String)
case class OEmail(email: Option[String])

case class Contact(
  firstname: String,
  lastname: String,
  company: Option[String],
  informations: Seq[ContactInformation]
)

case class ContactInformation(
  label: String,
  email: Option[String],
  phones: Seq[String]
)

class UFreeSpec extends WordSpec with Matchers {
  "UFree" should {
    implicit val freeMyCC1 =
      (
        (__ \ "int_field").as[Int] ~
        (__ \ "double_field").as[Double]
      ).as[MyCC1]

    implicitly[freeMyCC1.Implicits =:= (Int :: Double :: HNil)]
    val mycc2: MyCC2 = MyCC2(MyCC1(1, 2.2), true)

    implicit val freeUser = UFree.gen[User]()
    val freeDog  = UFree.gen[Dog]()
    implicitly[freeUser.Implicits =:= (Int :: String :: HNil)]
    implicitly[freeDog.Implicits =:= (String :: Int :: String :: HNil)]

    val freeContactInformation = UFree.gen[ContactInformation]()
    val freeContact = UFree.gen[Contact]()
    implicitly[freeContactInformation.Implicits =:= (String :: Option[String] :: Seq[String] :: HNil)]
    implicitly[freeContact.Implicits =:= (String :: String :: Option[String] :: Seq[ContactInformation] :: HNil)]

    "roundtrip flat composed case classes" in {
      val ast2 =
        (
          (__).as[MyCC1] ~
          (__ \ "boolean_field").as[Boolean]
        ).as[MyCC2]

      val ast2Typed: UFree[MyCC2] = ast2
      implicitly[ast2.Implicits =:= (Int :: Double :: Boolean :: HNil)]

      val rule2: Rule[JValue, MyCC2]   = ast2.materialize[Rule[JValue, ?]]
      val write2: Write[MyCC2, JValue] = ast2.materialize[Write[?, JValue]]

      val json2: JValue = write2.writes(mycc2)

      println(json2)
      assert(json2 == JObject(Map(
        "int_field"     -> JNumber(1),
        "double_field"  -> JNumber(2.2),
        "boolean_field" -> JBoolean(true)
      )))

      val validated2 = rule2.validate(json2)
      assert(validated2 == Valid(mycc2))
    }

    "roundtrip nested composed case classes" in {
      val ast3 =
        (
          (__ \ "nested").as[MyCC1] ~
          (__ \ "boolean_field").as[Boolean]
        ).as[MyCC2]

      val ast3Typed: UFree[MyCC2] = ast3
      implicitly[ast3.Implicits =:= (Int :: Double :: Boolean :: HNil)]

      val rule3: Rule[JValue, MyCC2]   = ast3.materialize[Rule[JValue, ?]]
      val write3: Write[MyCC2, JValue] = ast3.materialize[Write[?, JValue]]

      val mycc3: MyCC2 = MyCC2(MyCC1(1, 2.2), true)
      val json3: JValue = write3.writes(mycc2)

      println(json3)
      assert(json3 == JObject(Map(
        "nested" -> JObject(Map(
          "int_field"    -> JNumber(1),
          "double_field" -> JNumber(2.2)
        )),
        "boolean_field" -> JBoolean(true)
      )))

      val validated3 = rule3.validate(json3)

      println(validated3)
      assert(validated3 == Valid(mycc2))
    }

    "roundtrip macro generated case classes" in {
      val ast4 = UFree.gen[MyCC2]()

      val ast4Typed: UFree[MyCC2] = ast4
      implicitly[ast4.Implicits =:= (Int :: Double :: Boolean :: HNil)]

      val rule4: Rule[JValue, MyCC2]   = ast4.materialize[Rule[JValue, ?]]
      val write4: Write[MyCC2, JValue] = ast4.materialize[Write[?, JValue]]

      val mycc4: MyCC2 = MyCC2(MyCC1(1, 2.2), true)
      val json4: JValue = write4.writes(mycc2)

      println(json4)
      assert(json4 == JObject(Map(
        "mycc" -> JObject(Map(
          "int_field"    -> JNumber(1),
          "double_field" -> JNumber(2.2)
        )),
        "boolean" -> JBoolean(true)
      )))

      val validated4 = rule4.validate(json4)

      println(validated4)
      assert(validated4 == Valid(mycc2))
    }

    "handle option fields" in {
      val freeOEmail = UFree.gen[OEmail]()

      import Writes._
      val w: Write[OEmail, JValue] = freeOEmail.materialize[Write[?, JValue]]
      w.writes(OEmail(Some("Hello World"))) shouldBe JObject(Map("email" -> JString("Hello World")))
      w.writes(OEmail(None)) shouldBe JObject(Map())

      import Rules._
      val r: Rule[JValue, OEmail] = freeOEmail.materialize[Rule[JValue, ?]]
      r.validate(JObject(Map("email" -> JString("Hello World")))) shouldBe Valid(OEmail(Some("Hello World")))
      r.validate(JObject(Map())) shouldBe Valid(OEmail(None))
    }

    "create a Rule[User]" in {
      import Rules._
      implicit val userReads = freeUser.materialize[Rule[JValue, ?]]
      userReads.validate(JObject(Map("name" -> JString("toto"), "age" -> JNumber(45)))) shouldBe (Valid(User(45, "toto")))
    }

    "create a Write[User]" in {
      import Writes._
      implicit val userWrites = freeUser.materialize[Write[?, JValue]]
      userWrites.writes(User(45, "toto")) shouldBe (JObject(Map("name" -> JString("toto"), "age" -> JNumber(45))))
    }

    "create a Rule[Dog]" in {
      import Rules._
      implicit val userRule = freeUser.materialize[Rule[JValue, ?]]
      implicit val dogRule = freeDog.materialize[Rule[JValue, ?]]
      dogRule.validate(JObject(Map("name" -> JString("medor"), "master" -> JObject(Map("name" -> JString("toto"), "age" -> JNumber(45)))))) shouldBe (Valid(Dog("medor", User(45, "toto"))))
    }

    "create a Write[Dog]" in {
      import Writes._
      implicit val userWrite = freeUser.materialize[Write[?, JValue]]
      implicit val dogWrite = freeDog.materialize[Write[?, JValue]]
      dogWrite.writes(Dog("medor", User(45, "toto"))) shouldBe (JObject(Map("name" -> JString("medor"), "master" -> JObject(Map("name" -> JString("toto"), "age" -> JNumber(45))))))
    }

    "create a Write" in {
      implicit val contactInformationWrite: Write[ContactInformation, JValue] = freeContactInformation.materialize[Write[?, JValue]]
      implicit val contactWrite: Write[Contact, JValue] = freeContact.materialize[Write[?, JValue]]

      contactWrite.writes(
        Contact(
          "Julien",
          "Tournay",
          None,
          Seq(ContactInformation("Personal",
            Some("fakecontact@gmail.com"),
            Seq("01.23.45.67.89", "98.76.54.32.10"))))
      ) shouldBe JObject(Map(
        "firstname" -> JString("Julien"),
        "lastname" -> JString("Tournay"),
        "informations" -> JArray(Seq(JObject(Map(
          "label" -> JString("Personal"),
          "email" -> JString("fakecontact@gmail.com"),
          "phones" -> JArray(Seq(
            JString("01.23.45.67.89"),
            JString("98.76.54.32.10")))))))))
    }
  }
}
