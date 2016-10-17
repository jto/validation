package jto.validation
package jsjson

import scala.scalajs.js
import scala.util.Try

trait Rules extends DefaultRules[js.Dynamic] {
  private def jsonAs[T](
      f: PartialFunction[js.Any, Validated[Seq[ValidationError], T]])(
      msg: String, args: Any*) =
    Rule.fromMapping[js.Dynamic, T](f.orElse {
      case j => Invalid(Seq(ValidationError(msg, args: _*)))
    })

  implicit def stringR =
    jsonAs[String] {
      case v if (v: Any).isInstanceOf[String] => Valid(v.asInstanceOf[String])
    }("error.invalid", "String")

  implicit def booleanR =
    jsonAs[Boolean] {
      case v if v.isInstanceOf[Boolean] => Valid(v.asInstanceOf[Boolean])
    }("error.invalid", "Boolean")

  implicit def intR =
    jsonAs[Int] {
      case v if v.isInstanceOf[Int] => Valid(v.asInstanceOf[Int])
    }("error.number", "Int")

  implicit def shortR =
    jsonAs[Short] {
      case v if v.isInstanceOf[Short] => Valid(v.asInstanceOf[Short])
    }("error.number", "Short")

  implicit def longR =
    jsonAs[Long] {
      // Long are *opaque*, see http://www.scala-js.org/doc/semantics.html
      case v if js.typeOf(v) == "number" && Try(v.toString.toLong).isSuccess =>
        Valid(v.toString.toLong)
    }("error.number", "Long")

  implicit def jsObjectR =
    jsonAs[js.Dictionary[js.Dynamic]] {
      case v
          if v != null && js.typeOf(v) == "object" && !js.Array.isArray(v) =>
        Valid(v.asInstanceOf[js.Dictionary[js.Dynamic]])
    }("error.invalid", "Object")

  implicit def jsArrayR[A] =
    jsonAs[js.Array[A]] {
      case v: js.Array[_] => Valid(v.asInstanceOf[js.Array[A]])
    }("error.invalid", "Array")

  implicit def floatR =
    jsonAs[Float] {
      case v if v.isInstanceOf[Float] => Valid(v.asInstanceOf[Float])
    }("error.number", "Float")

  implicit def doubleR =
    jsonAs[Double] {
      case v if v.isInstanceOf[Double] => Valid(v.asInstanceOf[Double])
    }("error.number", "Double")

  implicit def bigDecimal =
    jsonAs[BigDecimal] {
      case v if Try(BigDecimal(v.toString)).isSuccess =>
        Valid(BigDecimal(v.toString))
    }("error.number", "BigDecimal")

  import java.{math => jm}
  implicit def javaBigDecimal =
    jsonAs[jm.BigDecimal] {
      case v if Try(new jm.BigDecimal(v.toString)).isSuccess =>
        Valid(new jm.BigDecimal(v.toString))
    }("error.number", "BigDecimal")

  implicit val jsNullR = jsonAs[Null] {
    case v if v == null => Valid(null)
  }("error.invalid", "null")

  implicit def ooo[O](
      p: Path)(implicit pick: Path => RuleLike[js.Dynamic, js.Dynamic],
               coerce: RuleLike[js.Dynamic, O]): Rule[js.Dynamic, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](
      r: => RuleLike[J, O], noneValues: RuleLike[js.Dynamic, js.Dynamic]*)(
      implicit pick: Path => RuleLike[js.Dynamic, js.Dynamic],
      coerce: RuleLike[js.Dynamic, J]): Path => Rule[js.Dynamic, Option[O]] =
    super.opt[J, O](r, (jsNullR.map(n => n: js.Dynamic) +: noneValues): _*)

  implicit def mapR[O](
      implicit r: RuleLike[js.Dynamic, O]): Rule[js.Dynamic, Map[String, O]] =
    super.mapR[js.Dynamic, O](r, jsObjectR.map(_.toSeq))

  implicit def jsDictToDyn[O](
      implicit r: RuleLike[js.Dictionary[js.Dynamic], O])
    : Rule[js.Dynamic, O] =
    jsObjectR.andThen(r)

  implicit def pickInJson[II <: js.Dynamic, O](p: Path)(
      implicit r: RuleLike[js.Dynamic, O]): Rule[II, O] = {
    def search(path: Path, json: js.Dynamic): Option[js.Dynamic] =
      path.path match {
        case KeyPathNode(k) :: t =>
          jsObjectR.validate(json).toOption.flatMap {
            obj: js.Dictionary[js.Dynamic] =>
              obj.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
          }

        case IdxPathNode(i) :: t =>
          jsArrayR.validate(json).toOption.flatMap {
            array: js.Array[js.Dynamic] =>
              array.lift(i).flatMap(j => search(Path(t), j))
          }

        case Nil => Some(json)
      }

    Rule[II, js.Dynamic] { json =>
      search(p, json) match {
        case None =>
          Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) => Valid(js)
      }
    }.andThen(r)
  }

  // XXX: a bit of boilerplate
  private def pickInS[T](
      implicit r: RuleLike[Seq[js.Dynamic], T]): Rule[js.Dynamic, T] =
    jsArrayR[js.Dynamic].map(fs => Seq(fs: _*)).andThen(r)
  implicit def pickSeq[O](implicit r: RuleLike[js.Dynamic, O]) =
    pickInS(seqR[js.Dynamic, O])
  implicit def pickSet[O](implicit r: RuleLike[js.Dynamic, O]) =
    pickInS(setR[js.Dynamic, O])
  implicit def pickList[O](implicit r: RuleLike[js.Dynamic, O]) =
    pickInS(listR[js.Dynamic, O])
  implicit def pickArray[O: scala.reflect.ClassTag](
      implicit r: RuleLike[js.Dynamic, O]) = pickInS(arrayR[js.Dynamic, O])
  implicit def pickTraversable[O](implicit r: RuleLike[js.Dynamic, O]) =
    pickInS(traversableR[js.Dynamic, O])
}

object Rules extends Rules
