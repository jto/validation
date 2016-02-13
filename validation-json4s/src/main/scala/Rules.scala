package jto.validation
package json4s

import org.json4s._

object Rules extends DefaultRules[JValue] {
  private def jsonAs[T](f: PartialFunction[JValue, Validated[Seq[ValidationError], T]])(msg: String, args: Any*) =
    Rule.fromMapping[JValue, T](
      f.orElse {
        case j => Invalid(Seq(ValidationError(msg, args: _*)))
      })

  implicit def stringR = jsonAs[String] {
    case JString(v) => Valid(v)
  }("error.invalid", "String")

  implicit def booleanR = jsonAs[Boolean] {
    case JBool(v) => Valid(v)
  }("error.invalid", "Boolean")

  // Note: Mappings of JsNumber to Number are validating that the JsNumber is indeed valid
  // in the target type. i.e: JsNumber(4.5) is not considered parseable as an Int.
  implicit def intR = jsonAs[Int] {
    case JInt(v) if v.isValidInt => Valid(v.toInt)
  }("error.number", "Int")

  implicit def shortR = jsonAs[Short] {
    case JInt(v) if v.isValidShort => Valid(v.toShort)
  }("error.number", "Short")

  implicit def longR = jsonAs[Long] {
    case JInt(v) if v.isValidLong => Valid(v.toLong)
  }("error.number", "Long")

  implicit def jsDouble = jsonAs[JsonAST.JDouble] {
    case v: JsonAST.JDouble => Valid(v)
  }("error.number", "Number")
  
  implicit def jsDecimal = jsonAs[JsonAST.JDecimal] {
    case v: JsonAST.JDecimal => Valid(v)
  }("error.number", "Number")
  
  implicit def jsInt = jsonAs[JsonAST.JInt] {
    case v: JsonAST.JInt => Valid(v)
  }("error.number", "Number")
  
  implicit def jsBooleanR = jsonAs[JBool] {
    case v @ JBool(_) => Valid(v)
  }("error.invalid", "Boolean")

  implicit def jsStringR = jsonAs[JString] {
    case v @ JString(_) => Valid(v)
  }("error.invalid", "String")

  implicit def jsObjectR = jsonAs[JObject] {
    case v @ JObject(_) => Valid(v)
  }("error.invalid", "Object")

  implicit def jsArrayR = jsonAs[JArray] {
    case v @ JArray(_) => Valid(v)
  }("error.invalid", "Array")

  // BigDecimal.isValidFloat is buggy, see [SI-6699]
  import java.{ lang => jl }
  private def isValidFloat(bd: BigDecimal) = {
    val d = bd.toFloat
    !d.isInfinity && bd.bigDecimal.compareTo(new java.math.BigDecimal(jl.Float.toString(d), bd.mc)) == 0
  }
  implicit def floatR = jsonAs[Float] {
    case JDecimal(v) if isValidFloat(v) => Valid(v.toFloat)
    case JDouble(v) if isValidFloat(v) => Valid(v.toFloat)
    case JInt(v) => Valid(v.toFloat)
  }("error.number", "Float")

  // BigDecimal.isValidDouble is buggy, see [SI-6699]
  private def isValidDouble(bd: BigDecimal) = {
    val d = bd.toDouble
    !d.isInfinity && bd.bigDecimal.compareTo(new java.math.BigDecimal(jl.Double.toString(d), bd.mc)) == 0
  }
  implicit def doubleR = jsonAs[Double] {
    case JDecimal(v) if isValidDouble(v) => Valid(v.toDouble)
    case JDouble(v) => Valid(v)
    case JInt(v) => Valid(v.toDouble)
  }("error.number", "Double")

  implicit def bigDecimal = jsonAs[BigDecimal] {
    case JDecimal(v) => Valid(v)
    case JDouble(v) => Valid(v)
    case JInt(v) => Valid(BigDecimal(v))
  }("error.number", "BigDecimal")

  import java.{ math => jm }
  implicit def javaBigDecimal = jsonAs[jm.BigDecimal] {
    case JDecimal(v) => Valid(v.bigDecimal)
    case JDouble(v) => Valid(BigDecimal(v).bigDecimal)
    case JInt(v) => Valid(BigDecimal(v).bigDecimal)
  }("error.number", "BigDecimal")

  implicit val jsNullR: Rule[JValue, JNull.type] = jsonAs[JNull.type] {
    case JNull => Valid(JNull)
  }("error.invalid", "null")

  implicit def ooo[O](p: Path)(implicit pick: Path => RuleLike[JValue, JValue], coerce: RuleLike[JValue, O]): Rule[JValue, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[JValue, JValue]*)(implicit pick: Path => RuleLike[JValue, JValue], coerce: RuleLike[JValue, J]): Path => Rule[JValue, Option[O]] =
    super.opt[J, O](r, (jsNullR.map(n => n: JValue) +: noneValues): _*)

  implicit def mapR[O](implicit r: RuleLike[JValue, O]): Rule[JValue, Map[String, O]] =
    super.mapR[JValue, O](r, jsObjectR.map { case JObject(fs) => fs })

  implicit def jsValueR[O](implicit r: RuleLike[JObject, O]): Rule[JValue, O] =
    jsObjectR.compose(r)

  implicit def pickInJson[II <: JValue, O](p: Path)(implicit r: RuleLike[JValue, O]): Rule[II, O] = {

    def search(path: Path, json: JValue): Option[JValue] = path.path match {
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

    Rule[II, JValue] { json =>
      search(p, json) match {
        case None => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) => Valid(js)
      }
    }.compose(r)
  }

  // // XXX: a bit of boilerplate
  private def pickInS[T](implicit r: RuleLike[Seq[JValue], T]): Rule[JValue, T] =
    jsArrayR.map { case JArray(fs) => Seq(fs:_*) }.compose(r)
  implicit def pickSeq[O](implicit r: RuleLike[JValue, O]) = pickInS(seqR[JValue, O])
  implicit def pickSet[O](implicit r: RuleLike[JValue, O]) = pickInS(setR[JValue, O])
  implicit def pickList[O](implicit r: RuleLike[JValue, O]) = pickInS(listR[JValue, O])
  implicit def pickArray[O: scala.reflect.ClassTag](implicit r: RuleLike[JValue, O]) = pickInS(arrayR[JValue, O])
  implicit def pickTraversable[O](implicit r: RuleLike[JValue, O]) = pickInS(traversableR[JValue, O])

}
