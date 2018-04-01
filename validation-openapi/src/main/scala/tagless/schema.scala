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

/**
  * @see https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#schemaObject
  */
sealed abstract class Type(val `type`: String) extends Property
object Type {
  def unapply(t: Type): Option[String] = Option(t.`type`)

  final case object Integer extends Type("integer")
  final case object Number extends Type("number")
  final case object String extends Type("string")
  final case object Boolean extends Type("boolean")
}

sealed trait Property

object Property {

  sealed abstract class Format[T <: Type](val format: String) extends Property
  final object Format {
    def unapply[T <: Type](f: Format[T]): Option[String] = Option(f.format)
  }
  final case object Int32 extends Format[Type.Integer.type]("int32")
  final case object Int64 extends Format[Type.Integer.type]("int64")
  final case object Float extends Format[Type.Number.type]("float")
  final case object Double extends Format[Type.Number.type]("double")
  final case object Byte extends Format[Type.String.type]("byte")
  final case object Binary extends Format[Type.String.type]("binary")
  final case object Date extends Format[Type.String.type]("date")
  final case object `Date-time` extends Format[Type.String.type]("date-time")
  final case object Password extends Format[Type.String.type]("password")
  final case class Raw[T <: Type](f: String) extends Format[T](f)

  final case class MinLength(l: Int) extends Property
  final case class MaxLength(l: Int) extends Property
  final case class Min(l: BigDecimal) extends Property
  final case class Max(l: BigDecimal) extends Property
  final case class Pattern(r: scala.util.matching.Regex) extends Property

  // TODO:
  // title
  // multipleOf
  // exclusiveMaximum
  // exclusiveMinimum
  // maxItems
  // minItems
  // uniqueItems
  // maxProperties
  // minProperties
  // enum

  // allOf
  // oneOf
  // anyOf
  // not
  // items
  // additionalProperties
  // description
  // default
}

sealed trait IsRequired
final case object Required extends IsRequired
final case object Optional extends IsRequired

sealed trait UntypedSchema {
  import io.swagger.util.{Yaml, Json}
  import com.fasterxml.jackson.databind.node.ObjectNode

  def java = SchemaOps.aSchema(this)

  // TODO: use a better type than String
  def yaml: String = {
    val o = Yaml.mapper.convertValue(java, classOf[ObjectNode])
    Yaml.pretty(o)
  }

  def json: String = {
    val o = Json.mapper.convertValue(java, classOf[ObjectNode])
    Json.pretty(o)
  }
}

sealed trait Schema[X, A] extends UntypedSchema
object Schema {
  def typed[X, A](t: Type, ps: Property*) =
    tag[Root](SPrimitive[X, A](Optional, t, ps.toList))
  def prop[X, A](ps: Property*) =
    tag[Root](Properties[X, A](ps.toList))
}

final private[openapi] case class Loc[X, A](path: Path) extends Schema[X, A]
final case class Properties[X, A](properties: List[Property])
    extends Schema[X, A]

final case class Forall[F[_], X, A](s: Schema[X, A]) extends Schema[F[X], F[A]]

sealed trait Typed[X, A] extends Schema[X, A] {
  def isRequired: IsRequired
}

object Typed {
  def unapply[X, A](t: Typed[X, A]): Option[(IsRequired, Typed[X, A])] =
    Option((t.isRequired, t))
}

final case class SPrimitive[X, A](isRequired: IsRequired,
                                  `type`: Type,
                                  properties: List[Property])
    extends Typed[X, A]
final case class SObject[X, A](isRequired: IsRequired,
                               children: List[(String, Typed[_, _])],
                               properties: List[Property])
    extends Typed[X, A]
final case class SArray[X, A, _X, _A](isRequired: IsRequired,
                                      child: Schema[_X, _A],
                                      properties: List[Property])
    extends Typed[X, A]

private[openapi] object SchemaOps {

  private def addProps[A](schema: ASchema[A], ps: List[Property]): ASchema[A] =
    ps.foldLeft[ASchema[A]](schema) {
      case (s, Property.Format(sn)) =>
        s.format(sn).asInstanceOf[ASchema[A]] // XXX: Cast because Java..
      case (s, Property.MinLength(l)) =>
        s.minLength(l).asInstanceOf[ASchema[A]] // XXX: Cast because Java..
      case (s, Property.MaxLength(l)) =>
        s.maxLength(l).asInstanceOf[ASchema[A]] // XXX: Cast because Java..
      case (s, Property.Min(l)) =>
        s.minimum(l.bigDecimal)
          .asInstanceOf[ASchema[A]] // XXX: Cast because Java..
      case (s, Property.Max(l)) =>
        s.maximum(l.bigDecimal)
          .asInstanceOf[ASchema[A]] // XXX: Cast because Java..
      case (s, Property.Pattern(r)) =>
        s.pattern(r.toString)
          .asInstanceOf[ASchema[A]] // XXX: Cast because Java..
    }

  def aSchema(sc: UntypedSchema): ASchema[_] = sc match {
    case SPrimitive(_, Type.Integer, ps) =>
      addProps(new IntegerSchema, ps)
    case SPrimitive(_, Type.Number, ps) =>
      addProps(new NumberSchema, ps)
    case SPrimitive(_, Type.String, ps) =>
      addProps(new StringSchema, ps)
    case SPrimitive(_, Type.Boolean, ps) =>
      addProps(new BooleanSchema, ps)
    case SObject(_, cs, ps) => {
      // TODO: add required properties
      val reqs =
        cs.collect {
          case ((n, Typed(Required, _))) =>
            n
        }
      val obj =
        cs.foldLeft[ASchema[_]]((new ObjectSchema)) {
          case (o, (n, c)) =>
            o.addProperties(n, aSchema(c))
        }

      val obj2 =
        reqs.foldLeft[ASchema[_]](obj) { (o, n) =>
          o.addRequiredItem(n)
        }
      addProps(obj2, ps)
    }
    case SArray(_, c, ps) =>
      addProps((new ArraySchema).items(aSchema(c)), ps)
    case sc =>
      throw new IllegalStateException(
        s"Can't convert validation's schema to a Java schema. (Schema was: $sc)")
  }
}
