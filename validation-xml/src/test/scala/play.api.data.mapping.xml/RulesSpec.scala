package play.api.data.mapping.xml

import org.specs2.mutable._
import play.api.data.mapping._
import scala.xml._

object RulesSpec extends Specification {

  "Xml rules" should {
    import play.api.data.mapping.xml.Rules._

    val valid =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations label="personal">
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.02</phone>
            <phone label="home">02.03</phone>
          </phones>
        </informations>
      </person>

    val invalid =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations label="">
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.02</phone>
            <phone label="home">02.03</phone>
          </phones>
        </informations>
      </person>

    "extract data" in {
      (Path \ "firstname").read[Node, String].validate(valid) === Success("Julien")
      val errPath = Path \ "foo"
      val error = Failure(Seq(errPath -> Seq(ValidationError("error.required"))))
      errPath.read[Node, String].validate(invalid) mustEqual(error)
    }

    "support attribute checked" in {
      val xml = <item checked="true">Item 1</item>
      attributeR[Boolean]("checked").validate(xml) === Success(true)
      attributeR[Boolean]("checked").validate(<empty></empty>) === Failure(Seq(Path -> Seq(ValidationError("error.required"))))
    }

    "support primitive types" in {

      "Int" in {
        Path.read[Node, Int].validate(<a>4</a>) === Success(4)
        attributeR("attr").validate(<a attr="4"></a>) === Success(4)
        Path.read[Node, Int].validate(<a>no</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        Path.read[Node, Int].validate(<a>4.8</a>) === Failure(Seq(Path -> Seq(ValidationError("error.number", "Int"))))
        (Path \ "b").read[Node, Int].validate(<a><b>4</b></a>) === Success(4)
        (Path \ "b").read[Node, Int](attributeR("attr")).validate(<a><b attr="4"></b></a>) === Success(4)
        (Path \ "b").read[Node, Int].validate(Json.obj("n" -> Json.obj("o" -> "foo"))) mustEqual(Failure(Seq(Path \ "n" \ "o" -> Seq(ValidationError("error.number", "Int")))))
      }

    }















    // -----

    val xmlNode =
      <person>
        <name>Alexandre</name>
        <age>25</age>
        <job type="fulltime">Software engineer</job>
        <phones>
          <phone type="mobile">01 123</phone>
          <phone type="home">02 123</phone></phones>
      </person>


    "extract a path" in {
      val reads = From[Node] { __ =>
        import Rules._
        (
         (__ \ "name").read[String] ~
         (__ \ "age").read[Int]
        ) tupled
      }
      reads.validate(xmlNode) === Success(("Alexandre", 25))
    }

    "fail when extracting a wrong path" in {
      val reads = From[Node] { __ =>
        import Rules._
        (
          (__ \ "name").read[String] ~
          (__ \ "address").read[String]
        ) tupled
      }

      reads.validate(xmlNode) === Failure(List((Path \ "address", List(ValidationError("error.required")))))
    } 

    "extract an attribute path" in {
      val reads = From[Node] { __ =>
        import Rules._
        (__ \ "job").read(attributeR[String]("type"))
      }
      reads.validate(xmlNode) === Success("fulltime")
    }

    "extract a sequence" in {
      val reads = From[Node] { __ =>
        import Rules._
        (__ \ "phones").read(pickSeq(
          attributeR[String]("type")
        ))
      }

      reads.validate(xmlNode) === Success(Seq("mobile", "home"))
    }

    "handle optional fields" in {
      val reads = From[Node] { __ =>
      import Rules._
      (
        (__ \ "address").read[Option[String]] ~
        (__ \ "age").read[Option[Int]]
      ) tupled
      }

      reads.validate(xmlNode) === Success((None, Some(25)))
    }

    "fail when trying to validate to string a non-leaf node" in {
      val reads = From[Node] { __ =>
        import Rules._
        (__ \ "phones").read[String]
      }

      reads.validate(xmlNode) === Failure(List((Path \ "phones",
        List(ValidationError("error.invalid", "a non-leaf node can not be validated to String")))))
    }


    val entityXml =
      <entity>
        <prop name="name" value="Alexandre"></prop>
        <prop name="age" value="25"></prop>
        <prop name="job" value="software engineer" type="fulltime"></prop>
      </entity>

    "use attribute filtering" in {
      val reads = From[Node] { __ =>
        import Rules._
        (
          pickChildWithAttribute("prop", attrKey = "name", attrValue = "job")(
            attributeR[String]("value") ~
            attributeR[String]("type")
            tupled
          ) ~
          pickChildWithAttribute("prop", attrKey = "name", attrValue = "age")(
            attributeR[Int]("value")
          )
        ) tupled
      }

      reads.validate(entityXml) === Success(("software engineer", "fulltime"), 25)
    }

  }
}
