package jto.validation
package jsonast

import jto.validation.Valid
import org.scalatest._

class AstSpec extends WordSpec with Matchers {
  "Ast" should {
    def prop(ast: JValue): Boolean =
      Ast.from.validate(Ast.to.writes(ast)) == Valid(ast)

    "be a bijection" in {
      val aNull = JNull
      val aString = JString("string")
      val aBoolean = JBoolean(true)
      val anArray = JArray(Vector(aBoolean, aString))
      val anObject = JObject(Map("a" -> anArray, "b" -> aBoolean))

      assert(prop(aNull))
      assert(prop(aString))
      assert(prop(aBoolean))
      assert(prop(anArray))
      assert(prop(anObject))
      assert(prop(JNumber("123.4")))
      assert(prop(JNumber("123")))
    }
  }
}
