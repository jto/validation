package jto.validation
package v3.tagless
package xml

import scala.xml.Node

class XMLWritesSpec extends WritesSpec[Node] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases

  object Yolo {
    import WritesGrammar2._
    def test1 = at(Path \ "foo" \ "bar")(req[String])

    def test2 = at(Path \ "foo" \ "bar")(opt[String])

    def test3 =
      at(Path \ "bar")(req[String]) ~:
      at(Path \ "foo")(opt[String]) ~:
      knil

    def test4 =
      at(Path \ "foo" \ "bar")(req(attr[Int]("id"))) ~:
      at(Path \ "baz")(req[String]) ~:
      knil

    def test5 =
      at(Path \ "foos"){
        req(list(at(Path \ "foo")(req[Int])))
      }

    import shapeless.{::, HList}
    implicit def mergeTC2: Merge[types.flip[Write]#λ, Option[XML]] =
      new Merge[types.flip[Write]#λ, Option[XML]] {
        def merge[A, B <: HList](fa: Write[A, Option[XML]], fb: Write[B, Option[XML]]): Write[A :: B, Option[XML]] =
          Write { case a :: b =>
            val wa = fa.writes(a)
            val wb = fb.writes(b)
            (wa, wb) match {
              case (None, None) => None
              case (None, b) => b
              case (a, None) => a
              case (Some(a), Some(b)) => Some(XML.Group(List(a, b)))
            }
          }
      }

    def kopt: Write[shapeless.HNil, Option[XML]] =
      Write { _ => None }

    def test6: Write[(String, Option[Int]), XML.Group[XML.At]] =
      at(Path \ "foo" \ "bar")(
        req[String] ~:
        opt(attr[Int]("id")) ~:
        kopt
      ).tupled

  }
}
