package jto.validation
package forms

import cats.Monoid

trait DefaultMonoids {
  implicit def mapMonoid = new Monoid[UrlFormEncoded] {
    def combine(a1: UrlFormEncoded, a2: UrlFormEncoded) = a1 ++ a2
    def empty = Map.empty
  }
}

trait Writes
    extends DefaultWrites
    with GenericWrites[PM.PM]
    with DefaultMonoids {
  import PM._

  implicit val intW: Write[Int, String] = Write(_.toString)
  implicit val shortW: Write[Short, String] = Write(_.toString)
  implicit val booleanW: Write[Boolean, String] = Write(_.toString)
  implicit val longW: Write[Long, String] = Write(_.toString)
  implicit val floatW: Write[Float, String] = Write(_.toString)
  implicit val doubleW: Write[Double, String] = Write(_.toString)
  implicit val bigDecimalW: Write[BigDecimal, String] = Write(_.toString)
  implicit def scalanumber[T <: scala.math.ScalaNumber] =
    Write((i: T) => i.toString)
  implicit def javanumber[T <: java.lang.Number] = Write((i: T) => i.toString)

  implicit def opm[O](implicit w: WriteLike[O, UrlFormEncoded]) =
    Write[O, PM] { o =>
      toPM(w.writes(o))
    }

  implicit def mapW[I](implicit w: WriteLike[I, Seq[String]]) =
    Write[Map[String, I], PM] { m =>
      toPM(m.mapValues(w.writes))
    }

  implicit def spm[O](implicit w: WriteLike[O, PM]) =
    Write[Seq[O], PM] { os =>
      os.zipWithIndex.map(_.swap).toMap.flatMap {
        case (i, o) =>
          repathPM(w.writes(o), (Path \ i) ++ _)
      }
    }

  implicit def writeM[I](path: Path)(implicit w: WriteLike[I, PM]) =
    Write[I, UrlFormEncoded] { i =>
      toM(repathPM(w.writes(i), path ++ _))
    }

  implicit def ospm[I](implicit w: WriteLike[I, String]) = Write[I, PM] { i =>
    Map(Path -> w.writes(i))
  }

  implicit def optW[I](implicit w: Path => WriteLike[I, UrlFormEncoded])
    : Path => Write[Option[I], UrlFormEncoded] =
    optionW[I, I](Write.zero[I])

  def optionW[I, J](r: => WriteLike[I, J])(
      implicit w: Path => WriteLike[J, UrlFormEncoded]) =
    super.optionW[I, J, UrlFormEncoded](r, Map.empty)
}

object Writes extends Writes
