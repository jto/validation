package jto.validation
package v3.tagless
package xml

import scala.xml.NodeSeq


class XMLRulesSpec extends RulesSpec[NodeSeq] {

  type From = NodeSeq

  val grammar = RulesGrammar
  val testCases = XMLTestCases
  val transform = x => x


  "Specific XML Rules" should {

    import grammar.{ map => _, _ }

    "List" in {
      import testCases.seq._
      at(Path \ "n")(req[List[String]])
        .validate(foos) shouldBe Valid(List("foo"))

      at(Path \ "n")(req[List[Int]])
        .validate(ints) shouldBe Valid(List(1, 2, 3))
    }

    // TODO: Add test case for index path node in RuleSpec
    "validate required attributes" in {
      import testCases.base
      val p = Path \ "phones" \ "phone"
      def r0 = at(p) |-> attr("label")
      def r1 = at(p) |-> attr("fake")
      r0(req(list[String])).validate(transform(base.info)) shouldBe Valid(List("mobile", "home"))
      r1(req(list[String])).validate(transform(base.info)) shouldBe
        Invalid(Seq(p \ "@fake" ->
          Seq(ValidationError("error.required"))))
    }

    "validate required attributes as Int" in {
      def r = (at(Path \ "test") |-> attr("label"))(req[Int])
      val xml = <test label="42"></test>
      r.validate(transform(xml)) shouldBe Valid(42)
      val xml2 = <test label="bar"></test>
      r.validate(transform(xml2)) shouldBe
        Invalid(Seq(Path \ "test" \ "@label" ->
          Seq(ValidationError("error.number", "Int"))))
    }

    "validate optional attributes" in {
      import testCases.base
      def r0 = (at(Path \ "phones" \ "phone") |-> attr("label"))(opt[String])
      def r1 = (at(Path \ "phones" \ "phone") |-> attr("fake"))(opt[String])
      r0.validate(transform(base.info)) shouldBe Valid(Option("mobile"))
      r1.validate(transform(base.info)) shouldBe Valid(None)
    }

  }
}

