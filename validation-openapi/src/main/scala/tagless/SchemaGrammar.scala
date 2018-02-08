package jto.validation
package v3.tagless
package openapi

import scala.reflect.ClassTag
import shapeless.{HList, HNil, Generic}
import shapeless.tag, tag.@@
import cats.Semigroup
import cats.arrow.Compose

import io.swagger.oas.models.media.{Schema => ASchema, _}

import types._

/*
import jto.validation._, v3.tagless._, openapi._
import cats.syntax.compose._
def info[T, K[_, _]](g: Grammar[T, K]) = {
  import g._
  at(Path \ "label").is(req[String]) ~:
  at(Path \ "email").is(opt(is[String] andThen email)) ~:
  at(Path \ "phones").is(req[String]) ~:
  knil
}
info(SchemaGrammar).yaml
 */
sealed trait SchemaGrammar extends Grammar[Nothing, Schema] {
  self =>

  type Sup = Nothing
  type Out = Nothing
  type P = SchemaGrammar

  def email: C[String] =
    Schema.prop(Property.Raw[Type.String.type]("email"))
  def equalTo[A](a: A): C[A] = ???
  def forall[I, O](k: Schema[I, O]): Schema[Seq[I], Seq[O]] = ???
  def max[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def maxLength(l: Int): C[String] = ???
  def min[A](a: A)(implicit O: Ordering[A]): C[A] = ???
  def minLength(l: Int): C[String] = ???
  def notEmpty: C[String] = ???
  def pattern(regex: scala.util.matching.Regex): C[String] = ???

  protected def asType[H, B](k: Schema[Out, H])(
      implicit G: Generic.Aux[B, H]): Schema[Out, B] =
    k match {
      case SPrimitive(r, t, ps) => SPrimitive(r, t, ps)
      case SObject(r, c, ps)    => SObject(r, c, ps)
      case SArray(r, c, ps)     => SArray(r, c, ps)
      case Loc(p)               => Loc(p)
      case Properties(ps)       => Properties(ps)
    }

  def at(p: Path): At[Schema, Out, Nothing] =
    new At[Schema, Out, Nothing] {
      def run: Schema[Out, Option[Nothing]] =
        Loc(p)
    }

  def is[A](implicit K: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, A] = K
  def knil: Schema[Out, HNil] = SObject(Optional, Nil, Nil)

  def mapPath(f: Path => Path): SchemaGrammar.P =
    new SchemaGrammar {
      override def at(p: Path) =
        self.at(f(p))
    }

  def opt[A](implicit K: Schema[_ >: Out <: Nothing, A])
    : Schema[Option[Nothing], Option[A]] =
    K match {
      case SPrimitive(_, t, ps1) =>
        SPrimitive(Optional, t, ps1)
      case SObject(_, cs, ps) =>
        SObject(Optional, cs, ps)
      case SArray(_, c, ps) =>
        SArray(Optional, c, ps)
      case _ =>
        throw new IllegalStateException(s"Impossible state in opt($K)")
    }

  def req[A](
      implicit K: Schema[_ >: Out <: Nothing, A]): Schema[Option[Nothing], A] =
    K match {
      case SPrimitive(_, t, ps1) =>
        SPrimitive(Required, t, ps1)
      case SObject(_, cs, ps) =>
        SObject(Required, cs, ps)
      case SArray(_, c, ps) =>
        SArray(Required, c, ps)
      case _ =>
        throw new IllegalStateException(s"Impossible state in req($K)")
    }

  implicit def string: Schema[Nothing, String] @@ Root =
    Schema.typed(Type.String)

  implicit def short: Schema[Nothing, Short] @@ Root =
    Schema.typed(Type.Integer, Property.Int32)
  implicit def int: Schema[Nothing, Int] @@ Root =
    Schema.typed(Type.Integer, Property.Int32)
  implicit def double: Schema[Nothing, Double] @@ Root =
    Schema.typed(Type.Number, Property.Double)
  implicit def float: Schema[Nothing, Float] @@ Root =
    Schema.typed(Type.Number, Property.Float)
  implicit def long: Schema[Nothing, Long] @@ Root =
    Schema.typed(Type.Integer, Property.Int64)
  implicit def bigDecimal: Schema[Nothing, BigDecimal] @@ Root =
    Schema.typed(Type.Number)
  implicit def jBigDecimal: Schema[Nothing, java.math.BigDecimal] @@ Root =
    Schema.typed(Type.Number)
  implicit def boolean: Schema[Nothing, Boolean] @@ Root =
    Schema.typed(Type.Boolean)

  implicit def array[A: ClassTag](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, Array[A]] =
    SArray(Optional, k, Nil)

  implicit def list[A](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, List[A]] =
    SArray(Optional, k, Nil)

  implicit def map[A](implicit k: Schema[_ >: Out <: Nothing, A])
    : Schema[Nothing, Map[String, A]] = ???

  implicit def seq[A](
      implicit k: Schema[_ >: Out <: Nothing, A]): Schema[Nothing, Seq[A]] =
    SArray(Optional, k, Nil)

  implicit def traversable[A](implicit k: Schema[_ >: Out <: Nothing, A])
    : Schema[Nothing, Traversable[A]] = SArray(Optional, k, Nil)

  private def objAt[X, A](p0: Path)(t0: Typed[_, _]): SObject[X, A] = {
    def go(pns: List[PathNode])(t: Typed[_, _]): SObject[X, A] =
      pns match {
        case KeyPathNode(h) :: Nil =>
          SObject(t.isRequired, List(h -> t), Nil)
        case KeyPathNode(h) :: hs =>
          SObject(t.isRequired, List(h -> go(hs)(t)), Nil)
        case IdxPathNode(h) :: _ =>
          throw new IllegalStateException(
            s"Illegal call to objAt: Cannot write at an index path")
        case Nil =>
          throw new IllegalStateException(
            s"Illegal call to objAt: Cannot write at the empty path")
      }
    go(p0.path)(t0)
  }

  // Members declared in jto.validation.v3.tagless.Typeclasses
  implicit def composeTC: Compose[Schema] =
    new Compose[Schema] {
      def compose[A, B, C0](f: Schema[B, C0], g: Schema[A, B]): Schema[A, C0] =
        (g, f) match {
          case (Loc(p1), Typed(_, t)) =>
            objAt(p1)(t)
          //
          case (SPrimitive(r, t, ps1), Properties(ps2)) =>
            SPrimitive(r, t, ps1 ++ ps2)
          case (SObject(r, cs, ps1), Properties(ps2)) =>
            SObject(r, cs, ps1 ++ ps2)
          case (SArray(r, c, ps1), Properties(ps2)) =>
            SArray(r, c, ps1 ++ ps2)
          //
          case (_, _) =>
            throw new IllegalStateException(
              s"Illegal composition: (f: $f, g: $g)")
        }
    }

  import shapeless.{:: => #:}
  implicit def mergeTC: Merge[Schema, Out] =
    new Merge[Schema, Out] {

      private def isRequired(i1: IsRequired, i2: IsRequired) =
        (i1, i2) match {
          case (Required, _) | (_, Required) => Required
          case _                             => Optional
        }

      private def collapse[A](o: SObject[Out, A]): SObject[Out, A] = {
        // XXX: try to preserve order ?
        val cs: List[(String, Typed[_, _])] =
          o.children
            .groupBy(_._1)
            .map {
              case (n, (_, s) :: Nil) =>
                (n, s)
              case (n, ss) =>
                // merge and recursivelly "collapse" objects
                // at the same location into a single object
                // If several Schema are at the same path, they must all be objects
                val nso =
                  ss.map(_._2)
                    .foldLeft[SObject[Out, _]](SObject(Optional, Nil, Nil)) {
                      case (SObject(r1, cs1, ps1), SObject(r2, cs2, ps2)) =>
                        SObject(isRequired(r1, r2), cs1 ++ cs2, ps1 ++ ps2)
                      case (x, y) =>
                        throw new IllegalStateException(
                          s"Incompatible Schemas at the same path. ($n) contains $x and $y")
                    }
                val collapsed = collapse(nso)
                (n, collapsed)
            }
            .toList

        SObject(o.isRequired, cs, o.properties)
      }

      private def deepMerge[A, B <: HList](
          o1: SObject[Out, A],
          o2: SObject[Out, B]): SObject[Out, A #: B] = {
        val r = isRequired(o1.isRequired, o2.isRequired)
        val o =
          SObject[Out, A #: B](r,
                               o1.children ++ o2.children,
                               o1.properties ++ o2.properties)
        collapse(o)
      }

      def merge[A, B <: HList](fa: Schema[Out, A],
                               fb: Schema[Out, B]): Schema[Out, A #: B] =
        (fa, fb) match {
          case (o1 @ SObject(_, _, _), o2 @ SObject(_, _, _)) =>
            deepMerge(o1, o2)
          case (a, b) =>
            throw new IllegalStateException(s"Can't merge $a with $b")
        }

    }

  implicit def mkLazy: MkLazy[Schema] =
    new MkLazy[Schema] {
      def apply[A, B](k: => Schema[A, B]): Schema[A, B] = k
    }

  implicit def semigroupTC[I0, O]: Semigroup[Schema[I0, O] @@ Root] =
    new Semigroup[Schema[I0, O] @@ Root] {
      def combine(x: Schema[I0, O] @@ Root,
                  y: Schema[I0, O] @@ Root): Schema[I0, O] @@ Root =
        (x: Schema[I0, O], y: Schema[I0, O]) match {
          case (Properties(ps1), Properties(ps2)) =>
            Schema.prop((ps1 ++ ps2): _*)
          case _ =>
            throw new IllegalStateException(s"Can't combine $x with $y")
        }
    }
}

object SchemaGrammar extends SchemaGrammar
