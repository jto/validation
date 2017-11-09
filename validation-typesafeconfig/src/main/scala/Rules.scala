package jto.validation
package typesafeconfig

import com.typesafe.config._

trait Rules extends DefaultRules[ConfigValue] {

  object Unwrapped {
    def unapply(cv: ConfigValue): Option[Any] = Option(cv.unwrapped)
  }

  object UnwrapNumber {
    def unapply(cv: ConfigValue): Option[BigDecimal] =
       PartialFunction.condOpt(cv) {
          case Unwrapped(n: Number) => BigDecimal(n.toString)
       }
  }

  private def configAs[T]
      (f: PartialFunction[ConfigValue, Validated[Seq[ValidationError], T]])
      (msg: String, args: Any*) =
    Rule.fromMapping[ConfigValue, T](f.orElse {
      case j => Invalid(Seq(ValidationError(msg, args: _*)))
    })

  implicit def stringR =
    configAs[String]{
      case Unwrapped(v: String) => Valid(v)
    }("error.invalid", "String")

  implicit def booleanR =
    configAs[Boolean] {
      case Unwrapped(v: Boolean) => Valid(v)
    }("error.invalid", "Boolean")

  // Note: Mappings to Number are validating that the Number is indeed valid
  // in the target type. i.e: Number(4.5) is not considered parseable as an Int.
  implicit def intR =
    configAs[Int] {
      case UnwrapNumber(v) if v.isValidInt => Valid(v.toInt)
    }("error.number", "Int")

  implicit def shortR =
    configAs[Short] {
      case UnwrapNumber(v) if v.isValidShort => Valid(v.toShort)
    }("error.number", "Short")

  implicit def longR =
    configAs[Long] {
      case UnwrapNumber(v) if v.isValidLong => Valid(v.toLong)
    }("error.number", "Long")

  implicit def floatR =
    configAs[Float] {
      case UnwrapNumber(v) if v.isDecimalFloat => Valid(v.toFloat)
    }("error.number", "Float")

  implicit def doubleR =
    configAs[Double] {
      case UnwrapNumber(v) if v.isDecimalDouble => Valid(v.toDouble)
    }("error.number", "Double")

  implicit def bigDecimal =
    configAs[BigDecimal] {
      case UnwrapNumber(v) => Valid(v)
    }("error.number", "BigDecimal")

  val nullR: Rule[ConfigValue, ConfigValue] = configAs[ConfigValue] {
    case cv if cv.valueType == ConfigValueType.NULL => Valid(cv)
  }("error.invalid", "null")

  implicit val noneR: Rule[ConfigValue, None.type] = nullR.map(_ => None)

  implicit def configObjectR =
    configAs[ConfigObject] {
      case co: ConfigObject => Valid(co)
    }("error.invalid", "Object")

  implicit def configListR = {
    import scala.collection.JavaConverters._
    configAs[Seq[ConfigValue]] {
      case cl: ConfigList => Valid(cl.asScala)
    }("error.invalid", "Array")
  }

  implicit def ooo[O](
      p: Path)(implicit pick: Path => RuleLike[ConfigValue, ConfigValue],
               coerce: RuleLike[ConfigValue, O]): Rule[ConfigValue, Option[O]] =
    optionR(Rule.zero[O])(pick, coerce)(p)

  def optionR[J, O](
      r: => RuleLike[J, O], noneValues: RuleLike[ConfigValue, ConfigValue]*)(
      implicit pick: Path => RuleLike[ConfigValue, ConfigValue],
      coerce: RuleLike[ConfigValue, J]): Path => Rule[ConfigValue, Option[O]] =
    super.opt[J, O](r, (nullR +: noneValues): _*)

  implicit def mapR[O](implicit r: RuleLike[ConfigValue, O]): Rule[ConfigValue, Map[String, O]] = {
    import scala.collection.JavaConverters._
    super.mapR[ConfigValue, O](r, configObjectR.map { case co => co.asScala.toSeq })
  }

  implicit def pickInJson[II <: ConfigValue, O](p: Path)
      (implicit r: RuleLike[ConfigValue, O]): Rule[II, O] = {

    def search(path: Path, json: ConfigValue): Option[ConfigValue] = path.path match {
      case KeyPathNode(k) :: t =>
        json match {
          case co: ConfigObject => Option(co.get(k)).flatMap { cv => search(Path(t), cv) }
          case _ => None
        }
      case IdxPathNode(i) :: t =>
        json match {
          case cl: ConfigList if cl.size > i => search(Path(t), cl.get(i))
          case _ => None
        }
      case Nil => Some(json)
    }

    Rule[II, ConfigValue] { json =>
      search(p, json) match {
        case None =>
          Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(js) => Valid(js)
      }
    }.andThen(r)
  }

  private def pickInS[T](
      implicit r: RuleLike[Seq[ConfigValue], T]): Rule[ConfigValue, T] =
    configListR.andThen(r)

  implicit def pickSeq[O](implicit r: RuleLike[ConfigValue, O]) =
    pickInS(seqR[ConfigValue, O])
  implicit def pickSet[O](implicit r: RuleLike[ConfigValue, O]) =
    pickInS(setR[ConfigValue, O])
  implicit def pickList[O](implicit r: RuleLike[ConfigValue, O]) =
    pickInS(listR[ConfigValue, O])
  implicit def pickArray[O: scala.reflect.ClassTag](
      implicit r: RuleLike[ConfigValue, O]) = pickInS(arrayR[ConfigValue, O])
  implicit def pickTraversable[O](implicit r: RuleLike[ConfigValue, O]) =
    pickInS(traversableR[ConfigValue, O])
}

object Rules extends Rules
