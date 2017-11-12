package jto.validation
package v3.tagless
package xml

import scala.xml.{NodeSeq, Null}

class XMLRulesSpec extends RulesSpec[List[XML]] {

  type From = NodeSeq

  val grammar = RulesGrammar
  val testCases = XMLTestCases
  val transform = x => List(Null -> x)

  "Specific XML Rules" should {

    import grammar.{ map => _, _ }

    "validate required attributes at root level" in {
      val xml = transform(<test label="bar"></test>)
      val rs0 = at(Path \ "test").is(req(attr("label").is(req[String])))
      val rs1 = at(Path \ "test").is(req(attr("fake").is(req[String])))
      rs0.validate(xml) shouldBe Valid("bar")
      rs1.validate(xml) shouldBe
        Invalid(Seq(Path \ "test" \ "@fake" ->
          Seq(ValidationError("error.required"))))
    }

    // TODO: Add test case for index path node in RuleSpec
    "validate required attributes" in {
      import testCases.base
      def r = at(Path \ "phones")

      def phone(s: String) =
        req(
          at(Path \ "phone").is(req(attr(s).is(req(list[String]))))
        )

      r.is(phone("label")).validate(transform(base.info)) shouldBe Valid(List("mobile", "home"))
      r.is(phone("fake")).validate(transform(base.info)) shouldBe
        Invalid(Seq(Path \ "phones" \ "phone" \ "@fake" ->
          Seq(ValidationError("error.required"))))
    }

    "validate required attributes AND node" in {
      import testCases.base

      val p = Path \ "phones" \ "phone"

      def r = at(p)
      val rs0 =
        is[String] ~:
        attr("label").is(req[String]) ~:
        knil

      val rule = r.is(req(rs0)).tupled

      rule.validate(transform(base.info)) shouldBe
        Valid(("01.23.45.67.89", "mobile"))

      val rs1 =
        is[String] ~:
        attr("fake").is(req[String]) ~:
        knil

      val attrErr =
        (p \ "@fake") -> Seq(ValidationError("error.required"))
      val nodeErr =
        p -> Seq(ValidationError("error.required"))

      r.is(req(rs1)).validate(transform(base.info)) shouldBe
        Invalid(Seq(attrErr))

      r.is(req(rs1)).validate(transform(NodeSeq.Empty)) shouldBe
        Invalid(Seq(nodeErr))
    }

    "validate required attributes as Int" in {
      def r = at(Path \ "test").is(req(attr("label").is(req[Int])))
      val xml = <test label="42"></test>
      r.validate(transform(xml)) shouldBe Valid(42)

      val xml2 = <test label="bar"></test>
      r.validate(transform(xml2)) shouldBe
        Invalid(Seq(Path \ "test" \ "@label" ->
          Seq(ValidationError("error.number", "Int"))))
    }

    "validate optional attributes" in {
      import testCases.base
      def r0 = at(Path \ "phones" \ "phone").is(req(attr("label").is(opt[String])))
      def r1 = at(Path \ "phones" \ "phone").is(req(attr("fake").is(opt[String])))
      r0.validate(transform(base.info)) shouldBe Valid(Option("mobile"))
      r1.validate(transform(base.info)) shouldBe Valid(None)
    }

  }
}


