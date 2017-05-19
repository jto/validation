package jto.validation
package v3.tagless
package xml

import scala.xml.Node

class XMLWritesSpec extends WritesSpec[Node] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases
}
