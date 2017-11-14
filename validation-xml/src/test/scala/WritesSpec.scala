import jto.validation._
import jto.validation.xml._
import jto.validation.xml.Writes._
import org.scalatest._
import scala.Function.unlift
import scala.Function.unlift

class WritesSpec extends WordSpec with Matchers {

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

  val contact = Contact(
    "Julien",
    "Tournay",
    None,
    Seq(
      ContactInformation("Personal",
                         Some("fakecontact@gmail.com"),
                         Seq("01.23.45.67.89", "98.76.54.32.10"))))

  "Writes" should {

    "write string" in {
      val w = (Path \ "label").write[String, XmlWriter]
      w.writes("Hello World")(<root></root>) shouldBe <root><label>Hello World</label></root>
    }

    "write string as an attribute" in {
      val w = (Path).write(attributeW[String]("attr"))
      w.writes("Hello World")(<root></root>) shouldBe <root attr="Hello World"></root>
    }

    "ignore values" in {
      (Path \ "n")
        .write(ignored("foo"))
        .writes("test")(<a></a>) shouldBe <a><n>foo</n></a>
      (Path \ "n")
        .write(ignored(42))
        .writes(0)(<a></a>) shouldBe <a><n>42</n></a>
    }

    "write an option" in {
      val w = To[XmlWriter] { __ =>
        ((__ \ "a").write[Option[Int]] ~ optAttributeW[Int]("b")) tupled
      }
      w.writes((Some(1), Some(2)))(<root></root>) shouldEqual <root b="2"><a>1</a></root>
      w.writes((Some(1), None))(<root></root>) shouldEqual <root><a>1</a></root>
      w.writes((None, Some(2)))(<root></root>) shouldEqual <root b="2"></root>
      w.writes((None, None))(<root></root>) shouldEqual <root></root>
    }

    "write a sequence" in {
      val s = Seq(1, 2, 3)
      val w = To[XmlWriter] { __ =>
        seqToNodeSeq(
          (__ \ "a").write[Int]
        )
      }
      w.writes(s)(<root></root>) shouldBe <root><a>1</a><a>2</a><a>3</a></root>
    }

    "write primitive types" when {

      "Int" in {
        Path.write[Int, XmlWriter].writes(4)(<a></a>) shouldBe (<a>4</a>)
        (Path \ "n" \ "o").write[Int, XmlWriter].writes(4)(<a></a>) shouldBe
          (<a><n><o>4</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Int, XmlWriter]
          .writes(4)(<a></a>) shouldBe
          (<a><n><o><p>4</p></o></n></a>)
      }

      "Short" in {
        (Path \ "n").write[Short, XmlWriter].writes(4)(<a></a>) shouldBe
          (<a><n>4</n></a>)
        (Path \ "n" \ "o").write[Short, XmlWriter].writes(4)(<a></a>) shouldBe
          (<a><n><o>4</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Short, XmlWriter]
          .writes(4)(<a></a>) shouldBe
          (<a><n><o><p>4</p></o></n></a>)
      }

      "Long" in {
        (Path \ "n").write[Long, XmlWriter].writes(4)(<a></a>) shouldBe
          (<a><n>4</n></a>)
        (Path \ "n" \ "o").write[Long, XmlWriter].writes(4)(<a></a>) shouldBe
          (<a><n><o>4</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Long, XmlWriter]
          .writes(4)(<a></a>) shouldBe
          (<a><n><o><p>4</p></o></n></a>)
      }

      "Float" in {
        (Path \ "n").write[Float, XmlWriter].writes(4.5f)(<a></a>) shouldBe
          (<a><n>4.5</n></a>)
        (Path \ "n" \ "o")
          .write[Float, XmlWriter]
          .writes(4.5f)(<a></a>) shouldBe
          (<a><n><o>4.5</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Float, XmlWriter]
          .writes(4.5f)(<a></a>) shouldBe
          (<a><n><o><p>4.5</p></o></n></a>)
      }

      "Double" in {
        (Path \ "n").write[Double, XmlWriter].writes(4.5d)(<a></a>) shouldBe
          (<a><n>4.5</n></a>)
        (Path \ "n" \ "o")
          .write[Double, XmlWriter]
          .writes(4.5d)(<a></a>) shouldBe
          (<a><n><o>4.5</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Double, XmlWriter]
          .writes(4.5d)(<a></a>) shouldBe
          (<a><n><o><p>4.5</p></o></n></a>)
      }

      "scala Big Decimal" in {
        (Path \ "n")
          .write[BigDecimal, XmlWriter]
          .writes(BigDecimal("4.0"))(<a></a>) shouldBe (<a><n>4.0</n></a>)
        (Path \ "n" \ "o")
          .write[BigDecimal, XmlWriter]
          .writes(BigDecimal("4.0"))(<a></a>) shouldBe
          (<a><n><o>4.0</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[BigDecimal, XmlWriter]
          .writes(BigDecimal("4.0"))(<a></a>) shouldBe
          (<a><n><o><p>4.0</p></o></n></a>)
      }

      "Boolean" in {
        (Path \ "n").write[Boolean, XmlWriter].writes(true)(<a></a>) shouldBe
          (<a><n>true</n></a>)
        (Path \ "n" \ "o")
          .write[Boolean, XmlWriter]
          .writes(false)(<a></a>) shouldBe
          (<a><n><o>false</o></n></a>)
        (Path \ "n" \ "o" \ "p")
          .write[Boolean, XmlWriter]
          .writes(true)(<a></a>) shouldBe (<a><n><o><p>true</p></o></n></a>)
      }
    }

    "compose with child nodes and/or attributes" in {
      val w = To[XmlWriter] { __ =>
        (
          (__ \ "firstname").write[String] ~
            (__ \ "age").write[Int]
        ) tupled
      }
      w.writes(("Julien", 28))(<user></user>) shouldBe
        <user><firstname>Julien</firstname><age>28</age></user>

      val w1 = To[XmlWriter] { __ =>
        (
          attributeW[String]("firstname") ~
            attributeW[Int]("age")
        ) tupled
      }
      w1.writes(("Julien", 28))(<user></user>) shouldBe
        <user firstname="Julien" age="28"></user>

      val w2 = To[XmlWriter] { __ =>
        (
          attributeW[String]("firstname") ~
            (__ \ "age").write[Int]
        ) tupled
      }
      w2.writes(("Julien", 28))(<user></user>) shouldBe
        <user firstname="Julien"><age>28</age></user>
    }

    "do a deep write" in {
      val w = To[XmlWriter] { __ =>
        (__ \ "a" \ "b").write(
          ((__ \ "c").write[String] ~ (__ \ "d").write(
            (__ \ "e").write[String]
          )) tupled
        )
      }
      w.writes(("foo", "bar"))(<root></root>) shouldBe <root><a><b><c>foo</c><d><e>bar</e></d></b></a></root>
    }

    "do a complex write" in {
      val w = To[XmlWriter] { __ =>
        ((__ \ "email").write[Option[String]] ~ (__ \ "phones").write(
          seqToNodeSeq(
            (__ \ "phone").write[String]
          ))) tupled
      }

      val v = Some("jto@foobar.com") -> Seq("01.23.45.67.89", "98.76.54.32.10")

      w.writes(v)(<a></a>) shouldBe <a><email>jto@foobar.com</email><phones><phone>01.23.45.67.89</phone><phone>98.76.54.32.10</phone></phones></a>
      w.writes(Some("jto@foobar.com") -> Nil)(<a></a>) shouldBe <a><email>jto@foobar.com</email><phones></phones></a>
      w.writes(None -> Nil)(<a></a>) shouldBe <a><phones></phones></a>
    }

    "write recursive" when {
      case class RecUser(name: String, friends: Seq[RecUser] = Nil)
      val u = RecUser("bob", Seq(RecUser("tom")))

      val m =
        <user><name>bob</name><friends><name>tom</name><friends></friends></friends></user>

      case class User1(name: String, friend: Option[User1] = None)
      val u1 = User1("bob", Some(User1("tom")))

      val m1 = <user><name>bob</name><friend><name>tom</name></friend></user>

      "using explicit notation" in {
        lazy val w: Write[RecUser, XmlWriter] = To[XmlWriter] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friends").write(seqW(w)))(
            unlift(RecUser.unapply))
        }
        w.writes(u)(<user></user>) shouldBe m

        lazy val w2: Write[RecUser, XmlWriter] =
          ((Path \ "name").write[String, XmlWriter] ~ (Path \ "friends").write(
            seqW(w2)))(unlift(RecUser.unapply))
        w2.writes(u)(<user></user>) shouldBe m

        lazy val w3: Write[User1, XmlWriter] = To[XmlWriter] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friend").write(optionW(w3)))(
            unlift(User1.unapply))
        }
        w3.writes(u1)(<user></user>) shouldBe m1
      }

      "using implicit notation" in {
        implicit lazy val w: Write[RecUser, XmlWriter] = To[XmlWriter] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friends").write[Seq[RecUser]])(
            unlift(RecUser.unapply))
        }
        w.writes(u)(<user></user>) shouldBe m

        implicit lazy val w3: Write[User1, XmlWriter] = To[XmlWriter] { __ =>
          ((__ \ "name").write[String] ~ (__ \ "friend").write[Option[User1]])(
            unlift(User1.unapply))
        }
        w3.writes(u1)(<user></user>) shouldBe m1
      }
    }
  }
}
