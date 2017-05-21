package jto.validation
package v3.tagless
package playjson

import play.api.libs.json.{JsValue, JsObject, JsNumber}
import jto.validation.playjson.Writes

object WriteTypeAlias {
  // I should be able to use flip[Write]#λ instead of creating this ugly
  // type alias but somehow, not naming the type breaks inference...
  type FWrite[A, B] = Write[B, A]
}

trait WritesGrammar
  extends JsonGrammar[WriteTypeAlias.FWrite]
  with WriteConstraints
  with WritesTypeclasses[JsValue] {
  self =>

  type Out = JsObject
  type P = WritesGrammar

  protected def iMonoid = Writes.jsonMonoid

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: JsValue]]): Write[A,JsObject] =
        self.at(f(p))(k)
    }

  def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: JsValue]]): Write[A, JsObject] =
    Write { t =>
      val js = k.writes(t)
      val w2 = Writes.optionW(Write.zero[JsValue])(Writes.writeJson _)(p)
      w2.writes(js)
    }

  def opt[A](implicit K: Write[A, _ >: Out <: JsValue]): Write[Option[A], Option[JsValue]] =
    Write {
      _.map(K.writes)
    }

  def req[A](implicit K: Write[A, _ >: Out <: JsValue]): Write[A, Option[JsValue]] =
    Write { a =>
      Option(K.writes(a))
    }

  implicit def int = Writes.intW
  implicit def string = Writes.string
  implicit def bigDecimal = Writes.bigDecimalW
  implicit def boolean = Writes.booleanW
  implicit def double = Writes.doubleW
  implicit def float = Writes.floatW
  implicit def jBigDecimal = Write { (j: java.math.BigDecimal) => JsNumber(j) }
  implicit def long = Writes.longW
  implicit def short = Writes.shortW

  implicit def seq[A](implicit k: Write[A, _ >: Out <: JsValue]) = Writes.seqToJsArray(k)
  implicit def list[A](implicit k: Write[A, _ >: Out <: JsValue]) = Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: JsValue]) = Writes.seqToJsArray(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: JsValue]) = Writes.mapW(k)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: JsValue]) = Writes.seqToJsArray(k).contramap(_.toSeq)

  implicit def jsNull = Write.zero
  implicit def jsObject = Write.zero
  implicit def jsString = Write.zero
  implicit def jsNumber = Write.zero
  implicit def jsBoolean = Write.zero

  def toGoal[Repr, A]: Write[Repr, Out] => Write[Goal[Repr, A], Out] =
    _.contramap{ _.value }
}

object WritesGrammar extends WritesGrammar
