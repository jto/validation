package jto.validation
package v3.tagless
package xml

import scala.xml.Node

class XMLRulesSpec extends RulesSpec[Node] {

  val grammar = RulesGrammar
  val testCases = XMLTestCases

  "Specific XML Rules" should {

    // import grammar.{ map => _, _ }

    // TODO: Add test case for index path node in RuleSpec
    // "validate required attributes" in {
    //   import testCases.base
    //   def r = at(Path \ "phones" \ "phone")(req[String].attr("label", req[String]))
    //   r.validate(base.info) shouldBe Valid("mobile")
    // }

    // "validate required attributes as Int" in {
    //   def r = at(Path \ "test")(req[String].attr("label", req[Int]))
    //   val xml = <root><test label="42"></test></root>
    //   r.validate(xml) shouldBe Valid(42)
    //   val xml2 = <root><test label="bar"></test></root>
    //   r.validate(xml2) shouldBe
    //     Invalid(Seq(Path \ "test" \ "@label" ->
    //       Seq(ValidationError("error.number", "Int"))))
    // }

    // "validate optional attributes" in {
    //   import testCases.base
    //   def r = at(Path \ "phones" \ "phone")(req[String].attr("fake", opt[String]))
    //   r.validate(base.info) shouldBe Valid(None)
    // }

  }
}
