package jto.validation
package v3.tagless
package playjson

import play.api.libs.json._

class JsonWritesSpec extends WritesSpec[JsValue] {
  type To = JsValue
  def transform = identity
  val grammar = WritesGrammar
  val testCases = JsonTestCases
}
