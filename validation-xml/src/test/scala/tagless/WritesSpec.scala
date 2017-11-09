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

    "write required attributes" in {
      import testCases.base
      val p = Path \ "phones" \ "phone"
      def r0 = at(p) |-> attr("label")
      val rs = req(list[String])
      transform(r0(rs).writes(List("mobile", "home"))) shouldBe base.info
    }

    "write required attributes AND node" in {
      import testCases.base
      val p = Path \ "phones" \ "phone"
      def w0 = at(p) |+> attr("label")
      val ws = zip(req[String], req[String])
      transform(w0(ws).writes(("01.23.45.67.89", "mobile"))) shouldBe base.info
    }

    "write required attributes as Int" in {
      def w = (at(Path \ "test") |-> attr("label")).apply(req[Int])
      val xml = <test label="42"></test> ++ NodeSeq.Empty
      transform(w.writes(42)) shouldBe xml
    }

    // "validate optional attributes" in {
    //   import testCases.base
    //   def r0 = (at(Path \ "phones" \ "phone") |-> attr("label")).apply(opt[String])
    //   def r1 = (at(Path \ "phones" \ "phone") |-> attr("fake")).apply(opt[String])
    //   r0.validate(transform(base.info)) shouldBe Valid(Option("mobile"))
    //   r1.validate(transform(base.info)) shouldBe Valid(None)
    // }

  }

}

