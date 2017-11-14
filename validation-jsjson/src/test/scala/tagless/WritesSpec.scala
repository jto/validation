package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js

class JsonWritesSpec extends WritesSpec[js.Dynamic] {
  type To = js.Dynamic
  def transform = identity
  val grammar = WritesGrammar
  val testCases = JsonTestCases
}
