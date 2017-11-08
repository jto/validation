package jto.validation
package v3.tagless
package xml

import scala.xml.{NodeSeq, MetaData, Attribute, Text, Null}

class XMLWritesSpec extends WritesSpec[Either[MetaData, NodeSeq]] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases

  type To = NodeSeq
  def transform = _.right.get ++ NodeSeq.Empty

  import grammar._

  "Specific XML Writes" should {

    "write required attributes at root level" in {
      def r0 = attr("label")
      val rs = req[String]
      r0(rs).writes("bar") shouldBe Left(Attribute("label", Text("bar"), Null))
    }

    // TODO: Add test case for index path node in RuleSpec
    // "validate required attributes" in {
    //   import testCases.base
    //   val p = Path \ "phones" \ "phone"
    //   def r0 = at(p) |-> attr("label")
    //   def r1 = at(p) |-> attr("fake")
    //   val rs = req(list[String])
    //   r0(rs).writes(List("mobile", "home")) shouldBe transform(base.info)
    // }

    // "validate required attributes as Int" in {
    //   def r = (at(Path \ "test") |-> attr("label")).apply(req[Int])
    //   val xml = <test label="42"></test>
    //   r.validate(transform(xml)) shouldBe Valid(42)

    //   val xml2 = <test label="bar"></test>
    //   r.validate(transform(xml2)) shouldBe
    //     Invalid(Seq(Path \ "test" \ "@label" ->
    //       Seq(ValidationError("error.number", "Int"))))
    // }

    // "validate optional attributes" in {
    //   import testCases.base
    //   def r0 = (at(Path \ "phones" \ "phone") |-> attr("label")).apply(opt[String])
    //   def r1 = (at(Path \ "phones" \ "phone") |-> attr("fake")).apply(opt[String])
    //   r0.validate(transform(base.info)) shouldBe Valid(Option("mobile"))
    //   r1.validate(transform(base.info)) shouldBe Valid(None)
    // }

  }

}

