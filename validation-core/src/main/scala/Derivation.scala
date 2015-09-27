package jto.validation

import shapeless.{Path => _, _}
import shapeless.labelled._
import cats.Monoid

trait DerivationInduction extends WriteProduct with WriteCoproduct with RuleProduct with RuleCoproduct {
  val typePath = Path \ "$type"
}

object DerivationRec extends WriteGeneric with RuleGeneric

trait RuleProduct {
  implicit def ruleProductBaseCase[I](p: Path): RuleLike[I, HNil] =
    new RuleLike[I, HNil] {
      def validate(i: I): VA[HNil] = Valid(HNil)
    }

  implicit def ruleProductInductionStep[I, K <: Symbol, V, T <: HList](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => RuleLike[I, V]],
      st: Lazy[Path => RuleLike[I, T]]
    ) =
    new RuleLike[I, FieldType[K, V] :: T] {
      def validate(i: I): VA[FieldType[K, V] :: T] = {
        val pathed = From[I] { __ =>
          (__ \ key.value.name).read[V](sv.value)
        }
        val head = pathed.validate(i)
        val tail = st.value(p).validate(i).map((t: T) => (v: V) => field[K](v) :: t)
        head.ap(tail)
      }
    }
}

trait RuleCoproduct {
  def typePath: Path

  implicit def ruleCoproductBaseCase[I](p: Path): RuleLike[I, CNil] =
    new RuleLike[I, CNil] {
      def validate(i: I): VA[CNil] =
        Invalid(Seq((typePath, Seq(ValidationError("Unknown $type")))))
    }

  implicit def ruleCoproductInductionStep[I, K <: Symbol, V, T <: Coproduct](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => RuleLike[I, V]],
      st: Lazy[Path => RuleLike[I, T]],
      rl: Path => RuleLike[I, String]
    ) =
    new RuleLike[I, FieldType[K, V] :+: T] {
      def validate(i: I): VA[FieldType[K, V] :+: T] = {
        typePath.read[I, String](rl).validate(i) match {
          case Valid(key.value.name) =>
            sv.value(p).validate(i).map(v => Inl(field[K](v)))
          case Valid(_) =>
            st.value(p).validate(i).map(Inr.apply)
          case _ =>
            Invalid(Seq((typePath, Seq(ValidationError("Missing $type")))))
        }
      }
    }
}

trait RuleGeneric {
  implicit def ruleGeneric[I, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => RuleLike[I, G]]
    ) =
    new RuleLike[I, F] {
      def validate(i: I): VA[F] = {
        sg.value(Path).validate(i).map(gen.from)
      }
    }
}

trait WriteProduct {
  type Output

  implicit def writeProductBaseCase(p: Path)(implicit m: Monoid[Output]) =
    new WriteLike[HNil, Output] {
      def writes(i: HNil): Output = m.empty
    }

  implicit def writeProductInductionStep[K <: Symbol, V, T <: HList](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => WriteLike[V, Output]],
      st: Lazy[Path => WriteLike[T, Output]],
      m: Monoid[Output]
    ) =
    new WriteLike[FieldType[K, V] :: T, Output] {
      def writes(i: FieldType[K, V] :: T): Output = {
        val pathed = To[Output] { __ =>
          (__ \ key.value.name).write[V](sv.value)
        }
        val head = pathed.writes(i.head)
        val tail = st.value(p).writes(i.tail)
        m.combine(head, tail)
      }
    }
}

trait WriteCoproduct {
  def typePath: Path
  type Output

  implicit val writeCoproductBaseCase =
    new WriteLike[CNil, Output] {
      def writes(i: CNil): Output = ???
    }

  implicit def writeCoproductInductionStep[K <: Symbol, V, T <: Coproduct](p: Path)
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Path => WriteLike[V, Output]],
      st: Lazy[Path => WriteLike[T, Output]],
      wl: Path => WriteLike[String, Output],
      m: Monoid[Output]
    ) =
    new WriteLike[FieldType[K, V] :+: T, Output] {
      def writes(i: FieldType[K, V] :+: T): Output = {
        i match {
          case Inl(v) =>
            val typeInfo = typePath.write(wl).writes(key.value.name)
            val value = sv.value(p).writes(v)
            m.combine(typeInfo, value)
          case Inr(t) =>
            st.value(p).writes(t)
        }
      }
    }
}

trait WriteGeneric {
  implicit def writeGeneric[O, F, G]
    (implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[Path => WriteLike[G, O]]
    ) =
    new WriteLike[F, O] {
      def writes(i: F): O = {
        sg.value(Path).writes(gen.to(i))
      }
    }
}
