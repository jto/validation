package jto.validation
package playjson

import play.api.libs.json.{JsValue, JsObject, Json, JsString, JsNumber, JsBoolean, JsArray}

trait DefaultMonoids {
  import cats.Monoid

  implicit def jsonMonoid = new Monoid[JsObject] {
    def combine(a1: JsObject, a2: JsObject): JsObject = a1 deepMerge a2
    def empty: JsObject = Json.obj()
  }
}

object Writes
    extends DefaultWrites
    with DefaultMonoids
    with GenericWrites[JsValue] {

  private def writeObj(j: JsValue, n: PathNode) = n match {
    case IdxPathNode(_) => Json.arr(j)
    case KeyPathNode(key) => Json.obj(key -> j)
  }

  implicit val validationErrorW = Write[ValidationError, JsValue] { err =>
    Json.obj("msg" -> JsString(err.message),
             "args" -> err.args.foldLeft(Json.arr()) { (arr, arg) =>
               arr :+
               (arg match {
                     case s: String => JsString(s)
                     case nb: Int => JsNumber(nb)
                     case nb: Short => JsNumber(nb)
                     case nb: Long => JsNumber(nb)
                     case nb: Double => JsNumber(nb)
                     case nb: Float => JsNumber(nb)
                     case b: Boolean => JsBoolean(b)
                     case js: JsValue => js
                     case x => JsString(x.toString)
                   })
             })
  }

  implicit def errorsW(
      implicit wErrs: WriteLike[Seq[ValidationError], JsValue]) =
    Write[(Path, Seq[ValidationError]), JsObject] {
      case (p, errs) =>
        Json.obj(p.toString -> wErrs.writes(errs))
    }

  implicit def failureW(
      implicit w: WriteLike[(Path, Seq[ValidationError]), JsObject]) =
    Write[Invalid[Seq[(Path, Seq[ValidationError])]], JsObject] {
      case Invalid(errs) =>
        errs.map(w.writes).reduce(_ ++ _)
    }

  implicit val string: Write[String, JsValue] = Write(s => JsString(s))

  private def tToJs[T] =
    Write[T, JsValue]((i: T) => JsNumber(BigDecimal(i.toString)))
  implicit def javanumber[T <: java.lang.Number] = tToJs[T]

  implicit val intW = tToJs[Int]
  implicit val shortW = tToJs[Short]
  implicit val longW = tToJs[Long]
  implicit val floatW = tToJs[Float]
  implicit val doubleW = tToJs[Double]

  implicit val bigDecimalW = Write[BigDecimal, JsValue](JsNumber.apply)

  implicit def booleanW = Write[Boolean, JsValue](JsBoolean.apply)

  implicit def seqToJsArray[I](
      implicit w: WriteLike[I, JsValue]): Write[Seq[I], JsValue] =
    Write(ss => JsArray(ss.map(w.writes _)))

  def optionW[I, J](r: => WriteLike[I, J])(
      implicit w: Path => WriteLike[J, JsObject])
    : Path => Write[Option[I], JsObject] =
    super.optionW[I, J, JsObject](r, Json.obj())

  implicit def optionW[I](implicit w: Path => WriteLike[I, JsObject])
    : Path => Write[Option[I], JsObject] =
    optionW(Write.zero[I])

  implicit def mapW[I](implicit w: WriteLike[I, JsValue]) =
    Write[Map[String, I], JsObject] { m =>
      JsObject(m.mapValues(w.writes).toSeq)
    }

  implicit def writeJson[I](path: Path)(
      implicit w: WriteLike[I, JsValue]): Write[I, JsObject] = Write { i =>
    path match {
      case Path(KeyPathNode(x) :: _) \: _ =>
        val ps = path.path.reverse
        val h = ps.head
        val o = writeObj(w.writes(i), h)
        ps.tail.foldLeft(o)(writeObj).asInstanceOf[JsObject]
      case _ =>
        throw new RuntimeException(s"path $path is not a path of JsObject") // XXX: should be a compile time error
    }
  }
}
