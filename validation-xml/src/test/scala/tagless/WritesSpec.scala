package jto.validation
package v3.tagless
package xml

import scala.xml.NodeSeq

class XMLWritesSpec extends WritesSpec[List[XML]] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases

  type To = NodeSeq
  def transform = xml => NodeSeq.fromSeq(xml.flatMap(_._2))

  import grammar._

  "Specific XML Writes" should {

    "write required attributes at root level" in {
      val out =
        at(Path \ "root").is(
          req(attr("label").is(req[String]))
        ).writes("bar")
      transform(out) shouldBe
        <root label="bar"></root> ++ NodeSeq.Empty
    }

    "write required attributes" in {
      val rs = req(attr("label").is(req[String]))
      val w =
        at(Path \ "phones").is(req(
          list(at(Path \ "phone").is(rs))
        ))
      transform(w.writes(List("mobile", "home"))) shouldBe
        <phones><phone label="mobile"></phone><phone label="home"></phone></phones> ++ NodeSeq.Empty
    }

    "write required attributes AND node" in {
      val p = Path \ "phones" \ "phone"
      def w = at(p)
      val ws =
        is[String] ~:
        attr("label").is(req[String]) ~:
        knil

      val d = List(("01.23.45.67.89", "mobile"), ("98.76.54.32.10", "home"))

      transform(w.is(req(ws)).tupled.writes(d.head)) shouldBe
        <phones><phone label="mobile">01.23.45.67.89</phone></phones> ++ NodeSeq.Empty

      val w2 =
        at(Path \ "phones").is(req(
          list(at(Path \ "phone").is(req(ws)).tupled)
        ))


      transform(w2.writes(d)) shouldBe
        <phones><phone label="mobile">01.23.45.67.89</phone><phone label="home">98.76.54.32.10</phone></phones> ++ NodeSeq.Empty
    }

    "write required attributes as Int" in {
      def w = at(Path \ "test").is(req(attr("label").is(req[Int])))
      val xml = <test label="42"></test> ++ NodeSeq.Empty
      transform(w.writes(42)) shouldBe xml
    }

    "write optional attributes" in {
      val r = at(Path \ "phones" \ "phone")
      val r0 = r.is(req(attr("label").is(opt[String])))
      val r1 = r.is(req(attr("fake").is(opt[String])))

      transform(r0.writes(Option("mobile"))) shouldBe
        <phones><phone label="mobile"></phone></phones> ++ NodeSeq.Empty

      transform(r0.writes(None)) shouldBe
        <phones><phone></phone></phones> ++ NodeSeq.Empty
    }

  }

}

