import org.scalatest._
import jto.validation._

class CaseClassTuplerSpec extends WordSpec with Matchers {
  "CaseClassTupler" should {
    "round trip for small example" in {
      val tupler = CaseClassTupler[Toto]
      implicitly[tupler.TupleRepr =:= (String, Int)]

      val toto  = Toto("s", 1)
      val tuple = ("s", 1)
      assert(tupler.to(toto)    == tuple)
      assert(tupler.from(tuple) == toto)
    }

    "round trip with many fields" in {
      val tupler = CaseClassTupler[X]
      type S = String
      implicitly[tupler.TupleRepr =:= (S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S)]

      val x = X("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
      val t =  ("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
      assert(tupler.to(x)   == t)
      assert(tupler.from(t) == x)
    }

    "single field case class" in {
      val tupler = CaseClassTupler[Cat]
      implicitly[tupler.TupleRepr =:= String]

      val cat   = Cat("s")
      val tuple = "s"
      assert(tupler.to(cat)    == tuple)
      assert(tupler.from(tuple) == cat)
    }
  }
}

case class Cat(name: String)

case class Toto(s: String, i: Int)

case class X(
  _1: String,
  _2: String,
  _3: String,
  _4: String,
  _5: String,
  _6: String,
  _7: String,
  _8: String,
  _9: String,
  _10: String,
  _11: String,
  _12: String,
  _13: String,
  _14: String,
  _15: String,
  _16: String,
  _17: String,
  _18: String,
  _19: String,
  _20: String,
  _21: String
)
