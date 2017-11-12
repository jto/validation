package jto.validation

import cats.Applicative
import cats.arrow.Arrow
import cats.syntax.cartesian._
import shapeless.tag, tag.@@

sealed trait RuleLike[I, O] {
  /**
    * Apply the Rule to `data`
    * @param data The data to validate
    * @return The Result of validating the data
    */
  def validate(data: I): VA[O] =
    validateWithPath(data).map(_._2)

  def validateWithPath(data: I): VA[(Path, O)]
}

object RuleLike {
  implicit def zero[O]: RuleLike[O, O] = Rule[O, O](o => Valid(Path -> o))
}

trait Rule[I, O] extends RuleLike[I, O] {

  def flatMap[B](f: O => Rule[I, B]): Rule[I, B] =
    Rule { d =>
      validateWithPath(d)
        .map { case (p, o) =>
          (p, f(o))
        }.fold(
          es => Invalid(es),
          { case (p, r) => r.repath(p ++ _).validateWithPath(d) }
        )
    }

  /**
    * Create a new Rule that try `this` Rule, and apply `t` if it fails
    * {{{
    *   val rb: Rule[JsValue, A] = From[JsValue]{ __ =>
    *     ((__ \ "name").read[String] ~ (__ \ "foo").read[Int])(B.apply)
    *   }
    *
    *   val rc: Rule[JsValue, A] = From[JsValue]{ __ =>
    *     ((__ \ "name").read[String] ~ (__ \ "bar").read[Int])(C.apply)
    *   }
    *   val rule = rb orElse rc orElse Rule(_ => typeInvalid)
    * }}}
    * @param t an alternative Rule
    * @return a Rule
    */
  def orElse[OO >: O](t: => RuleLike[I, OO]): Rule[I, OO] =
    Rule(d => this.validateWithPath(d) orElse t.validateWithPath(d))

  def andThen[P](sub: => RuleLike[O, P]): Rule[I, P] =
    flatMap { o =>
      Rule[I, P]{ _ =>
        sub.validateWithPath(o)
      }
    }

  def andThen[P](m: Mapping[ValidationError, O, P]): Rule[I, P] =
    andThen(Rule.fromMapping(m))

  /**
    * This methods allows you to modify the Path of errors (if the result is a Invalid) when aplying the Rule
    */
  def repath(f: Path => Path): Rule[I, O] =
    Rule{ i =>
      validateWithPath(i)
        .leftMap {
          _.map{ case (ep, errs) => (f(ep), errs) }
        }
        .map { case (p, o) => (f(p), o) }
    }

  def map[B](f: O => B): Rule[I, B] =
    Rule(d => this.validateWithPath(d).map{ case (p, o) => (p, f(o)) })

  def ap[A](mf: Rule[I, O => A]): Rule[I, A] =
    Rule { i=>
      val va = validateWithPath(i)
      val vf = mf.validateWithPath(i)
      (vf |@| va).map { (f, a) =>
        (a._1, f._2(a._2)) // XXX: Not sure which path to keep.
      }
    }

  def to[T](implicit g: shapeless.Generic.Aux[T, O]) =
    map(o => g.from(o))

  def tupled[T](implicit t: Tupler[O]) =
    map(o => t.from(o))
}

object Rule {

  def gen[I, O]: Rule[I, O] = macro MappingMacros.rule[I, O]

  /**
    * Turn a `A => Rule[B, C]` into a `Rule[(A, B), C]`
    * {{{
    *   val passRule = From[JsValue] { __ =>
    *      ((__ \ "password").read(notEmpty) ~ (__ \ "verify").read(notEmpty))
    *        .tupled .andThen(Rule.uncurry(Rules.equalTo[String]).repath(_ => (Path \ "verify")))
    *    }
    * }}}
    */
  def uncurry[A, B, C](f: A => Rule[B, C]): Rule[(A, B), C] =
    Rule { case (a, b) => f(a).validateWithPath(b) }

  def zero[O]: Rule[O, O] =
    toRule(RuleLike.zero[O])

  def pure[I, O](o: O): Rule[I, O] =
    Rule(_ => Valid((Path, o)))

  def apply[I, O](m: Mapping[(Path, Seq[ValidationError]), I, (Path, O)]): Rule[I, O] =
    new Rule[I, O] {
      def validateWithPath(data: I): VA[(Path, O)] = m(data)
    }

  def lazily[I, O](path: Path)(r: Path => RuleLike[I, O]): Rule[I, O] =
    Rule { i =>
      r(path).validateWithPath(i)
    }

  def of[I, O](implicit r: Rule[I, O]): Rule[I, O] = r

  def toRule[I, O](r: RuleLike[I, O]): Rule[I, O] =
    new Rule[I, O] {
      def validateWithPath(data: I): VA[(Path, O)] = r.validateWithPath(data)
    }

  def fromMapping[I, O](f: Mapping[ValidationError, I, O]): Rule[I, O] @@ Root =
    tag[Root](Rule[I, O]{ i =>
      f(i).bimap(
        errs => Seq(Path -> errs),
        o => (Path, o)
      )
    })

  implicit def applicativeRule[I]: Applicative[Rule[I, ?]] =
    new Applicative[Rule[I, ?]] {
      def pure[A](a: A): Rule[I, A] = Rule.pure(a)
      def ap[A, B](mf: Rule[I, A => B])(ma: Rule[I, A]): Rule[I, B] = ma.ap(mf)
    }

  implicit def ruleSyntaxCombine[I]: SyntaxCombine[Rule[I, ?]] =
    new SyntaxCombine[Rule[I, ?]] {
      def apply[A, B](a: Rule[I, A], b: Rule[I, B]): Rule[I, A ~ B] =
        b.ap(a.map(a => c => new ~(a, c)))
    }

  implicit def ruleSemigroup[I, O] =
    new cats.Semigroup[Rule[I, O] @@ Root] {
      def combine(r1: Rule[I,O] @@ Root, r2: Rule[I,O] @@ Root): Rule[I,O] @@ Root = {
        val r =
          Rule[I, O] { v =>
             (r1.validate(v) *> r2.validate(v)).bimap(
                 _.groupBy(_._1).map {
                   case (path, errs) =>
                     path -> errs.flatMap(_._2)
                 }.toSeq,
                 o => (Path, o)
             )
           }
        tag.apply(r)
      }
    }

  implicit def ruleFunctorSyntaxObs[I, O](
      r: Rule[I, O])(implicit fcb: SyntaxCombine[Rule[I, ?]])
    : FunctorSyntaxObs[Rule[I, ?], O] =
    new FunctorSyntaxObs[Rule[I, ?], O](r)(fcb)

  import cats.arrow.FunctionK
  implicit def ruleLiftFunctionK[Out]: FunctionK[Rule[?, Option[Out]], λ[α => Rule[Option[α],Option[Out]]]] =
    new FunctionK[Rule[?, Option[Out]], λ[α => Rule[Option[α],Option[Out]]]] {
      def apply[A](fa: Rule[A, Option[Out]]): Rule[Option[A], Option[Out]] =
        Rule { ma =>
          ma.map { a =>
            fa.validateWithPath(a)
          }.getOrElse {
            Valid((Path, None))
          }
        }
    }

  implicit def ruleArrow =
    new Arrow[Rule] {
      def first[A, B, C](fa: Rule[A, B]): Rule[(A, C), (B, C)] =
        Rule { case (a, c) =>
          fa.map{ (_, c) }.validateWithPath(a)
        }

      def lift[A, B](f: A => B): Rule[A, B] =
        Rule.fromMapping(a => Valid(f(a)))

      def id[A]: Rule[A, A] = Rule.zero

      def compose[A, B, C](f: Rule[B, C],g: Rule[A,B]): Rule[A, C] =
        g andThen f
    }

  implicit def mkLazyRule =
    new v3.tagless.MkLazy[Rule] {
      def apply[A, B](k: => Rule[A, B]): Rule[A, B] =
        Rule { a => k.validateWithPath(a) }
    }
}

object Read {
  sealed trait Deferred[O] {
    def apply[I](i: I)(implicit r: RuleLike[I, O]) = r.validate(i)
  }

  def apply[O] = new Deferred[O]{}
}
