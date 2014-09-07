package play.api.data.mapping.xml

import org.specs2.mutable._
import play.api.data.mapping._
import scala.xml._

object RulesSpec extends Specification {
  "Xml rules" should {

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
        (__ \ "job").read(attributeR("type"))
      }
      reads.validate(xmlNode) === Success("fulltime")
    }

    "extract a sequence" in {
      val reads = From[Node] { __ =>
        import Rules._
        (__ \ "phones").read(pickSeq(
          __.read(attributeR("type"))
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
            __.read(attributeR("value")) ~
            __.read(attributeR("type"))
            tupled
          ) ~
          pickChildWithAttribute("prop", attrKey = "name", attrValue = "age")(
            __.read(attributeIntR("value"))
          )
        ) tupled
      }

      reads.validate(entityXml) === Success(("software engineer", "fulltime"), 25)
    }

  }
}
