package jto.validation
package v3.tagless
package xml

import scala.xml.NodeSeq


class XMLRulesSpec extends RulesSpec[XML.T] {

  type From = NodeSeq

  val grammar = RulesGrammar
  val testCases = XMLTestCases
  val transform = XML.fromScala _


  "Specific XML Rules" should {

    import grammar.{ map => _, _ }

    // TODO: Add test case for index path node in RuleSpec
    "validate required attributes" in {
      import testCases.base
      val p = Path \ "phones" \ "phone"
      def r0 = at(p)(req(attr[String]("label")))
      def r1 = at(p)(req(attr[String]("fake")))
      r0.validate(transform(base.info)) shouldBe Valid("mobile")
      r1.validate(transform(base.info)) shouldBe
        Invalid(Seq(p \ "@fake" ->
          Seq(ValidationError("error.required"))))
    }

    "validate required attributes as Int" in {
      def r = at(Path \ "test")(req(attr[Int]("label")))
      val xml = <test label="42"></test>
      r.validate(transform(xml)) shouldBe Valid(42)
      val xml2 = <test label="bar"></test>
      r.validate(transform(xml2)) shouldBe
        Invalid(Seq(Path \ "test" \ "@label" ->
          Seq(ValidationError("error.number", "Int"))))
    }

    "validate optional attributes" in {
      import testCases.base
      def r0 = at(Path \ "phones" \ "phone")(opt(attr[String]("label")))
      def r1 = at(Path \ "phones" \ "phone")(opt(attr[String]("fake")))
      r0.validate(transform(base.info)) shouldBe Valid(Option("mobile"))
      // r1.validate(transform(base.info)) shouldBe Valid(None)
    }

  }
}


