package jto.validation
package v3.tagless
package jsjson

import scala.scalajs.js
import org.scalactic.{Equality, Prettifier}

class JsonWritesSpec extends WritesSpec[js.Dynamic] {
  type To = js.Dynamic
  def transform = identity
  val grammar = WritesGrammar
  val testCases = JsonTestCases

  override implicit def prettifier: Prettifier =
    new Prettifier {
      def apply(o: Any) =
        js.JSON.stringify(o.asInstanceOf[To])
    }

  override implicit def equality: Equality[To] =
    new Equality[To] {
      def areEqual(a: To, b: Any) =
        js.JSON.stringify(a) == js.JSON.stringify(b.asInstanceOf[js.Any])
    }
}
