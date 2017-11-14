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

    val info =
      <label>Personal</label>
      <email>fakecontact@gmail.com</email>
      <phones>
        <phone label="mobile">01.23.45.67.89</phone>
        <phone label="home">98.76.54.32.10</phone>
      </phones>

    import grammar.{map => _, _}

    "validate required attributes at root level" in {
      val xml = transform(<test label="bar"></test>)
      val rs0 = at(Path \ "test").is(req(attr("label").is(req[String])))
      val rs1 = at(Path \ "test").is(req(attr("fake").is(req[String])))
      rs0.validate(xml) shouldBe Valid("bar")
      rs1.validate(xml) shouldBe
        Invalid(
          Seq(Path \ "test" \ "@fake" ->
            Seq(ValidationError("error.required"))))
    }

    // TODO: Add test case for index path node in RuleSpec
    "validate list of required attributes" in {

      def phone(s: String) =
        at(Path \ "phones").is(
          req(
            at(Path \ "phone").is(
              req(
                list(attr(s).is(req[String]))
              ))
          ))

      phone("label").validate(transform(info)) shouldBe
        Valid(List("mobile", "home"))

      phone("fake").validate(transform(info)) shouldBe
        Invalid(
          Seq(
            (Path \ "phones" \ "phone" \ 0 \ "@fake") -> Seq(
              ValidationError("error.required")),
            (Path \ "phones" \ "phone" \ 1 \ "@fake") -> Seq(
              ValidationError("error.required"))
          ))
    }

    "validate required attributes AND node" in {

      val p = Path \ "phones" \ "phone"

      val rs0 =
        is[String] ~:
          attr("label").is(req[String]) ~:
          knil

      val ruleOK =
        at(Path \ "phones").is(
          req(
            at(Path \ "phone").is(req(rs0.tupled))
          ))

      ruleOK.validate(transform(info)) shouldBe
        Valid(("01.23.45.67.89", "mobile"))

      val rs1 =
        is[String] ~:
          attr("fake").is(req[String]) ~:
          knil

      val ruleNOK =
        at(Path \ "phones").is(
          req(
            at(Path \ "phone").is(req(rs1.tupled))
          ))

      val attrErr =
        (p \ "@fake") -> Seq(ValidationError("error.required"))

      val nodeErr =
        Path \ "phones" -> Seq(ValidationError("error.required"))

      ruleNOK.validate(transform(info)) shouldBe
        Invalid(Seq(attrErr))

      ruleNOK.validate(transform(NodeSeq.Empty)) shouldBe
        Invalid(Seq(nodeErr))
    }

    "validate required attributes as Int" in {
      def r = at(Path \ "test").is(req(attr("label").is(req[Int])))
      val xml = <test label="42"></test>
      r.validate(transform(xml)) shouldBe Valid(42)

      val xml2 = <test label="bar"></test>
      r.validate(transform(xml2)) shouldBe
        Invalid(
          Seq(Path \ "test" \ "@label" ->
            Seq(ValidationError("error.number", "Int"))))
    }

    "validate optional attributes" in {
      def r0 =
        at(Path \ "phones" \ "phone").is(req(attr("label").is(opt[String])))
      def r1 =
        at(Path \ "phones" \ "phone").is(req(attr("fake").is(opt[String])))
      r0.validate(transform(info)) shouldBe Valid(Option("mobile"))
      r1.validate(transform(info)) shouldBe Valid(None)
    }

  }
}
