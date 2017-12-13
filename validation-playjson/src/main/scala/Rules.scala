package jto.validation
package playjson

import shapeless.tag.@@
import play.api.libs.json.{
  JsValue,
  JsObject,
  JsString,
  JsNumber,
  JsBoolean,
  JsArray,
  JsNull
}

trait Rules extends DefaultRules[JsValue] {

  private def jsonAs[T](
      f: PartialFunction[JsValue, Validated[Seq[ValidationError], T]])(
      msg: String,
      args: Any*) =
    Rule.fromMapping[JsValue, T](f.orElse {
      case _ => Invalid(Seq(ValidationError(msg, args: _*)))
    })

  implicit def stringR =
    jsonAs[String] {
      case JsString(v) => Valid(v)
    }("error.invalid", "String")

  implicit def booleanR =
    jsonAs[Boolean] {
      case JsBoolean(v) => Valid(v)
    }("error.invalid", "Boolean")

  // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  // That's a bit stricter than the "old" Read, which just cast to the target type, possibly loosing data.
  implicit def intR =
    jsonAs[Int] {
      case JsNumber(v) if v.isValidInt => Valid(v.toInt)
    }("error.number", "Int")

  implicit def shortR =
    jsonAs[Short] {
      case JsNumber(v) if v.isValidShort => Valid(v.toShort)
    }("error.number", "Short")

  implicit def longR =
    jsonAs[Long] {
      case JsNumber(v) if v.isValidLong => Valid(v.toLong)
    }("error.number", "Long")

  implicit def jsNumberR =
    jsonAs[JsNumber] {
      case v @ JsNumber(_) => Valid(v)
    }("error.number", "Number")

  implicit def jsBooleanR =
    jsonAs[JsBoolean] {
      case v @ JsBoolean(_) => Valid(v)
    }("error.invalid", "Boolean")

  implicit def jsStringR =
    jsonAs[JsString] {
      case v @ JsString(_) => Valid(v)
    }("error.invalid", "String")

  implicit def jsObjectR =
    jsonAs[JsObject] {
      case v @ JsObject(_) => Valid(v)
    }("error.invalid", "Object")

  implicit def jsArrayR =
    jsonAs[JsArray] {
      case v @ JsArray(_) => Valid(v)
    }("error.invalid", "Array")

  implicit def floatR =
    jsonAs[Float] {
      case JsNumber(v) if v.isDecimalFloat => Valid(v.toFloat)
    }("error.number", "Float")

  implicit def doubleR =
    jsonAs[Double] {
      case JsNumber(v) if v.isDecimalDouble => Valid(v.toDouble)
    }("error.number", "Double")

  implicit def bigDecimal =
    jsonAs[BigDecimal] {
      case JsNumber(v) => Valid(v)
    }("error.number", "BigDecimal")

  import java.{math => jm}
  implicit def javaBigDecimal =
    jsonAs[jm.BigDecimal] {
      case JsNumber(v) => Valid(v.bigDecimal)
    }("error.number", "BigDecimal")

  implicit val jsNullR: Rule[JsValue, JsNull.type] @@ Root =
    jsonAs[JsNull.type] {
      case JsNull => Valid(JsNull)
    }("error.invalid", "null")

  implicit def ooo[O](p: Path)(
      implicit pick: Path => RuleLike[JsValue, JsValue],
      coerce: RuleLike[JsValue, O]): Rule[JsValue, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](r: => RuleLike[J, O],
                    noneValues: RuleLike[JsValue, JsValue]*)(
      implicit pick: Path => RuleLike[JsValue, JsValue],
      coerce: RuleLike[JsValue, J]): Path => Rule[JsValue, Option[O]] =
    super.opt[J, O](r, (jsNullR.map(n => n: JsValue) +: noneValues): _*)

  implicit def mapR[O](
      implicit r: RuleLike[JsValue, O]): Rule[JsValue, Map[String, O]] =
    super.mapR[JsValue, O](r, jsObjectR.map { case JsObject(fs) => fs.toSeq })

  implicit def JsValue[O](implicit r: RuleLike[JsObject, O]): Rule[JsValue, O] =
    jsObjectR.andThen(r)

  implicit def pickInJson[II <: JsValue, O](p: Path)(
      implicit r: RuleLike[JsValue, O]): Rule[II, O] = {

    def search(path: Path, json: JsValue): Option[JsValue] = path.path match {
      case KeyPathNode(k) :: t =>
        json match {
          case JsObject(js) =>
            js.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
          case _ => None
        }
      case IdxPathNode(i) :: t =>
        json match {
          case JsArray(js) => js.lift(i).flatMap(j => search(Path(t), j))
          case _           => None
        }
      case Nil => Some(json)
    }

    Rule[II, JsValue] { (json: II) =>
      search(p, json) match {
        case None =>
          Invalid(Seq(p -> Seq(ValidationError("error.required"))))
        case Some(js) =>
          Valid(p -> js)
      }
    }.andThen(r)
  }

  private def pickInS[T](
      implicit r: RuleLike[Seq[JsValue], T]): Rule[JsValue, T] =
    jsArrayR.map { case JsArray(fs) => fs.toSeq }.andThen(r)
  implicit def pickSeq[O](implicit r: RuleLike[JsValue, O]) =
    pickInS(seqR[JsValue, O])
  implicit def pickSet[O](implicit r: RuleLike[JsValue, O]) =
    pickInS(setR[JsValue, O])
  implicit def pickList[O](implicit r: RuleLike[JsValue, O]) =
    pickInS(listR[JsValue, O])
  implicit def pickArray[O: scala.reflect.ClassTag](
      implicit r: RuleLike[JsValue, O]) = pickInS(arrayR[JsValue, O])
  implicit def pickTraversable[O](implicit r: RuleLike[JsValue, O]) =
    pickInS(traversableR[JsValue, O])
}

object Rules extends Rules
