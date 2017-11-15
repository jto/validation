package jto.validation
package jsonast

import shapeless.tag.@@

trait Rules extends DefaultRules[JValue] {
  private def jsonAs[T](
      f: PartialFunction[JValue, Validated[Seq[ValidationError], T]])(
      msg: String, args: Any*): Rule[JValue, T] @@ Root =
    Rule.fromMapping[JValue, T](f.orElse {
      case j => Invalid(Seq(ValidationError(msg, args: _*)))
    })

  implicit def stringR =
    jsonAs[String] {
      case JString(v) => Valid(v)
    }("error.invalid", "String")

  implicit def booleanR =
    jsonAs[Boolean] {
      case JBoolean(v) => Valid(v)
    }("error.invalid", "Boolean")

  // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  implicit def intR =
    jsonAs[Int] {
      case JNumber(v) if BigDecimal(v).isValidInt => Valid(v.toInt)
    }("error.number", "Int")

  implicit def shortR =
    jsonAs[Short] {
      case JNumber(v) if BigDecimal(v).isValidShort => Valid(v.toShort)
    }("error.number", "Short")

  implicit def longR =
    jsonAs[Long] {
      case JNumber(v) if BigDecimal(v).isValidLong => Valid(v.toLong)
    }("error.number", "Long")

  implicit def jsNumberR =
    jsonAs[JNumber] {
      case v @ JNumber(_) => Valid(v)
    }("error.number", "Number")

  implicit def jsBooleanR =
    jsonAs[JBoolean] {
      case v @ JBoolean(_) => Valid(v)
    }("error.invalid", "Boolean")

  implicit def jsStringR =
    jsonAs[JString] {
      case v @ JString(_) => Valid(v)
    }("error.invalid", "String")

  implicit def jsObjectR =
    jsonAs[JObject] {
      case v @ JObject(_) => Valid(v)
    }("error.invalid", "Object")

  implicit def jsArrayR =
    jsonAs[JArray] {
      case v @ JArray(_) => Valid(v)
    }("error.invalid", "Array")

  implicit def floatR =
    jsonAs[Float] {
      case JNumber(v) if BigDecimal(v).isDecimalFloat => Valid(v.toFloat)
    }("error.number", "Float")

  implicit def doubleR =
    jsonAs[Double] {
      case JNumber(v) if BigDecimal(v).isDecimalDouble => Valid(v.toDouble)
    }("error.number", "Double")

  implicit def bigDecimal =
    jsonAs[BigDecimal] {
      case JNumber(v) => Valid(BigDecimal(v))
    }("error.number", "BigDecimal")

  import java.{math => jm}
  implicit def javaBigDecimal =
    jsonAs[jm.BigDecimal] {
      case JNumber(v) => Valid(BigDecimal(v).bigDecimal)
    }("error.number", "BigDecimal")

  implicit val jsNullR =
    jsonAs[JNull.type] {
      case JNull => Valid(JNull)
    }("error.invalid", "null")

  implicit def ooo[O](
      p: Path)(implicit pick: Path => RuleLike[JValue, JValue],
               coerce: RuleLike[JValue, O]): Rule[JValue, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](
      r: => RuleLike[J, O], noneValues: RuleLike[JValue, JValue]*)(
      implicit pick: Path => RuleLike[JValue, JValue],
      coerce: RuleLike[JValue, J]): Path => Rule[JValue, Option[O]] =
    super.opt[J, O](r, (jsNullR.map(n => n: JValue) +: noneValues): _*)

  implicit def mapR[O](
      implicit r: RuleLike[JValue, O]): Rule[JValue, Map[String, O]] =
    super.mapR[JValue, O](r, jsObjectR.map { case JObject(fs) => fs.toSeq })

  implicit def jsValueR[O](implicit r: RuleLike[JObject, O]): Rule[JValue, O] =
    jsObjectR.andThen(r)

  @inline private def search(path: Path, json: JValue): Option[JValue] =
    path.path match {
      case KeyPathNode(k) :: t =>
        json match {
          case JObject(js) =>
            js.find(_._1 == k).flatMap(kv => search(Path(t), kv._2))
          case _ => None
        }
      case IdxPathNode(i) :: t =>
        json match {
          case JArray(js) => js.lift(i).flatMap(j => search(Path(t), j))
          case _ => None
        }
      case Nil => Some(json)
    }

  implicit def pickInJson[II <: JValue, O](p: Path)(implicit r: RuleLike[JValue, O]): Rule[II, O] =
    Rule[II, Option[JValue]] { json =>
      Valid(p -> search(p, json))
    }.andThen(required[JValue]).andThen(r)


  // XXX: a bit of boilerplate
  private def pickInS[T](
      implicit r: RuleLike[Seq[JValue], T]): Rule[JValue, T] =
    jsArrayR.map { case JArray(fs) => Seq(fs: _*) }.andThen(r)
  implicit def pickSeq[O](implicit r: RuleLike[JValue, O]) =
    pickInS(seqR[JValue, O])
  implicit def pickSet[O](implicit r: RuleLike[JValue, O]) =
    pickInS(setR[JValue, O])
  implicit def pickList[O](implicit r: RuleLike[JValue, O]) =
    pickInS(listR[JValue, O])
  implicit def pickArray[O: scala.reflect.ClassTag](
      implicit r: RuleLike[JValue, O]) = pickInS(arrayR[JValue, O])
  implicit def pickTraversable[O](implicit r: RuleLike[JValue, O]) =
    pickInS(traversableR[JValue, O])
}

object Rules extends Rules
