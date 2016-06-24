package jto.validation
package jsonast

import cats.Monoid

trait DefaultMonoids {
  implicit def jsonMonoid = new Monoid[JObject] {
    // TODO: Should this be a deepMerge?
    def combine(a1: JObject, a2: JObject) = JObject(a1.value ++ a2.value)
    def empty = JObject()
  }
}

object Writes
    extends DefaultWrites
    with DefaultMonoids
    with GenericWrites[JValue] {
  private def writeObj(j: JValue, n: PathNode) = n match {
    case IdxPathNode(_) => JArray(Vector(j))
    case KeyPathNode(key) => JObject(Map(key -> j))
  }

  implicit val validationErrorW = Write[ValidationError, JValue] { err =>
    JObject(
        Map("msg" -> JString(err.message),
            "args" -> err.args.foldLeft(JArray()) { (arr, arg) =>
          JArray((arr.value :+ JString(arg.toString)).toVector)
        }))
  }

  implicit def errorsW(
      implicit wErrs: WriteLike[Seq[ValidationError], JValue]) =
    Write[(Path, Seq[ValidationError]), JObject] {
      case (p, errs) =>
        JObject(Map(p.toString -> wErrs.writes(errs)))
    }

  implicit def failureW(
      implicit w: WriteLike[(Path, Seq[ValidationError]), JObject]) =
    Write[Invalid[Seq[(Path, Seq[ValidationError])]], JObject] {
      case Invalid(errs) =>
        errs.map(w.writes).reduce(jsonMonoid.combine)
    }

  implicit val stringW: Write[String, JValue] = Write(s => JString(s))

  private def tToJs[T]: Write[T, JValue] =
    Write[T, JValue](i => JNumber(i.toString))

  implicit val intW = tToJs[Int]
  implicit val shortW = tToJs[Short]
  implicit val longW = tToJs[Long]
  implicit val floatW = tToJs[Float]
  implicit val doubleW = tToJs[Double]
  implicit val bigDecimalW: Write[BigDecimal, JValue] =
    Write[BigDecimal, JValue](b => JNumber(b.toString))
  implicit def javanumberW[T <: java.lang.Number] = tToJs[T]

  implicit def booleanW = Write[Boolean, JValue](JBoolean.apply)

  implicit def seqToJsArray[I](
      implicit w: WriteLike[I, JValue]): Write[Seq[I], JValue] =
    Write(ss => JArray(ss.map(w.writes _).toVector))

  def optionW[I, J](r: => WriteLike[I, J])(
      implicit w: Path => WriteLike[J, JObject])
    : Path => Write[Option[I], JObject] =
    super.optionW[I, J, JObject](r, JObject())

  implicit def optionW[I](implicit w: Path => WriteLike[I, JObject])
    : Path => Write[Option[I], JObject] =
    optionW(Write.zero[I])

  implicit def mapW[I](implicit w: WriteLike[I, JValue]) =
    Write[Map[String, I], JObject] { m =>
      JObject(m.mapValues(w.writes))
    }

  implicit def vaW[I](implicit w: WriteLike[I, JValue]) =
    Write[VA[I], JObject] { va =>
      JObject(
          Map(
              "isValid" -> JBoolean(va.isValid),
              "output" -> va.fold(_ => JNull, w.writes),
              "errors" -> va.fold(e => failureW.writes(Invalid(e)), _ => JNull)
          ))
    }

  implicit def writeJson[I](path: Path)(
      implicit w: WriteLike[I, JValue]): Write[I, JObject] = Write { i =>
    path match {
      case Path(KeyPathNode(x) :: _) \: _ =>
        val ps = path.path.reverse
        val h = ps.head
        val o = writeObj(w.writes(i), h)
        ps.tail.foldLeft(o)(writeObj).asInstanceOf[JObject]
      case Path(Nil) =>
        w.writes(i).asInstanceOf[JObject]
      case _ =>
        throw new RuntimeException(s"path $path is not a path of JsObject") // XXX: should be a compile time error
    }
  }
}
