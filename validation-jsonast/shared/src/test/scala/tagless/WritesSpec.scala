package jto.validation
package v3.tagless
package jsonast

import jto.validation.jsonast._

class JsonWritesSpec extends jto.validation.v3.tagless.WritesSpec[JValue] {
  type To = JValue
  def transform = identity _
  val grammar = WritesGrammar
  val testCases = JsonTestCases
}
