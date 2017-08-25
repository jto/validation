package jto.validation
package v3.tagless
package xml

import scala.xml.Node

class XMLWritesSpec extends WritesSpec[XML] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases

  type To = Node
  def transform = _.asInstanceOf[XML.Group[XML.At]].build

  import grammar._

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

  def test6: Write[(String, Option[Int]), XML.Group[XML.At]] =
    at(Path \ "foo" \ "bar")(
      req[String] ~:
      opt(attr[Int]("id")) ~:
      kopt
    ).tupled


}
