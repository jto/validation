package jto.validation
package playjson

import play.api.libs.json.{JsValue, JsObject, JsString, JsNumber, JsBoolean, JsArray, JsNull}
import scala.reflect.ClassTag

object Rules extends DefaultRules[JsValue] {
  private def jsonAs[T](
      f: PartialFunction[JsValue, Validated[Seq[ValidationError], T]])(
      msg: String, args: Any*): Rule[JsValue, T] =
    Rule.fromMapping[JsValue, T](f.orElse {
      case j => Invalid(Seq(ValidationError(msg, args: _*)))
    })

  implicit def stringR: Rule[JsValue, String] =
    jsonAs[String] {
      case JsString(v) => Valid(v)
    }("error.invalid", "String")

  implicit def booleanR: Rule[JsValue, Boolean] =
    jsonAs[Boolean] {
      case JsBoolean(v) => Valid(v)
    }("error.invalid", "Boolean")

  // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  // That's a bit stricter than the "old" Read, which just cast to the target type, possibly loosing data.
  implicit def intR: Rule[JsValue, Int] =
    jsonAs[Int] {
      case JsNumber(v) if v.isValidInt => Valid(v.toInt)
    }("error.number", "Int")

  implicit def shortR: Rule[JsValue, Short] =
    jsonAs[Short] {
      case JsNumber(v) if v.isValidShort => Valid(v.toShort)
    }("error.number", "Short")

  implicit def longR: Rule[JsValue, Long] =
    jsonAs[Long] {
      case JsNumber(v) if v.isValidLong => Valid(v.toLong)
    }("error.number", "Long")

  implicit def jsNumberR: Rule[JsValue, JsNumber] =
    jsonAs[JsNumber] {
      case v @ JsNumber(_) => Valid(v)
    }("error.number", "Number")

  implicit def jsBooleanR: Rule[JsValue, JsBoolean] =
    jsonAs[JsBoolean] {
      case v @ JsBoolean(_) => Valid(v)
    }("error.invalid", "Boolean")

  implicit def jsStringR: Rule[JsValue, JsString] =
    jsonAs[JsString] {
      case v @ JsString(_) => Valid(v)
    }("error.invalid", "String")

  implicit def jsObjectR: Rule[JsValue, JsObject] =
    jsonAs[JsObject] {
      case v @ JsObject(_) => Valid(v)
    }("error.invalid", "Object")

  implicit def jsArrayR: Rule[JsValue, JsArray] =
    jsonAs[JsArray] {
      case v @ JsArray(_) => Valid(v)
    }("error.invalid", "Array")

  implicit def floatR: Rule[JsValue, Float] =
    jsonAs[Float] {
      case JsNumber(v) if v.isDecimalFloat => Valid(v.toFloat)
    }("error.number", "Float")

  implicit def doubleR: Rule[JsValue, Double] =
    jsonAs[Double] {
      case JsNumber(v) if v.isDecimalDouble => Valid(v.toDouble)
    }("error.number", "Double")

  implicit def bigDecimal: Rule[JsValue, BigDecimal] =
    jsonAs[BigDecimal] {
      case JsNumber(v) => Valid(v)
    }("error.number", "BigDecimal")

  implicit def javaBigDecimal: Rule[JsValue, java.math.BigDecimal] =
    jsonAs[java.math.BigDecimal] {
      case JsNumber(v) => Valid(v.bigDecimal)
    }("error.number", "BigDecimal")

  implicit def jsNullR: Rule[JsValue, JsNull.type] =
    jsonAs[JsNull.type] {
      case JsNull => Valid(JsNull)
    }("error.invalid", "null")

  implicit def mapR[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Map[String, O]] =
    super.mapR[JsValue, O](r, jsObjectR.map { case JsObject(fs) => fs.toSeq })

  implicit def JsValue[O](implicit r: Rule[JsObject, O]): Rule[JsValue, O] =
    jsObjectR.andThen(r)

  private def pickInS[T](implicit r: Rule[Seq[JsValue], T]): Rule[JsValue, T] =
    jsArrayR.map { case JsArray(fs) => fs }.andThen(r)

  implicit def pickSeq[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Seq[O]] =
    pickInS(seqR[JsValue, O])

  implicit def pickSet[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Set[O]] =
    pickInS(setR[JsValue, O])

  implicit def pickList[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, List[O]] =
    pickInS(listR[JsValue, O])

  implicit def pickArray[O: ClassTag](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Array[O]] =
    pickInS(arrayR[JsValue, O])

  implicit def pickTraversable[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Traversable[O]] =
    pickInS(traversableR[JsValue, O])

  implicit def optionR[O](
      implicit r: Rule[JsValue, O]): Rule[JsValue, Option[O]] =
    r.map(Some.apply)

  implicit val ruleAtJson: At[Rule[JsValue, ?]] = new At[Rule[JsValue, ?]] {
    def at[A: ClassTag](p: Path, r: Rule[JsValue, A]): Rule[JsValue, A] = {
      def search(path: Path, json: JsValue): Option[JsValue] =
        path.path match {
          case KeyPathNode(k) :: t =>
            json match {
              case JsObject(js) =>
                js.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
              case _ => None
            }

          case IdxPathNode(i) :: t =>
            json match {
              case JsArray(js) => js.lift(i).flatMap(j => search(Path(t), j))
              case _ => None
            }
          case Nil => Some(json)
        }

      val rule = Rule[JsValue, JsValue] { json =>
        search(p, json) match {
          case None =>
            Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
          case Some(js) => Valid(js)
        }
      }.andThen(r).repath(p ++ _)

      val option =
        implicitly[ClassTag[A]].runtimeClass == classOf[scala.Option[_]]

      if (option)
        rule.orElse(Rule(_ => Valid(None.asInstanceOf[A])))
      else
        rule
    }
  }
}
