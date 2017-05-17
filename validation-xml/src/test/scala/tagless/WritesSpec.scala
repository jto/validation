package jto.validation
package v3.tagless
package xml

import jto.validation.xml._

class XMLWritesSpec extends WritesSpec[XmlWriter] {
  val grammar = WritesGrammar
  val testCases = XMLTestCases
}
