package jto.validation

import cats.Applicative
import cats.syntax.cartesian._
import shapeless.tag, tag.@@

sealed trait RuleLike[I, O] {

  private[validation] def path: Path

  private[validation] def validateNoRoot(data: I): VA[O]

  /**
    * Apply the Rule to `data`
    * @param data The data to validate
    * @return The Result of validating the data
    */
  def validate(data: I): VA[O] =
    validateNoRoot(data).leftMap {
      _.map{ case (ep, errs) => (path ++ ep) -> errs }
    }

}

object RuleLike {
  implicit def zero[O]: RuleLike[O, O] = Rule[O, O](Path)(Valid.apply)
}

trait Rule[I, O] extends RuleLike[I, O] {

  def flatMap[B](f: O => Rule[I, B]): Rule[I, B] =
    Rule(path) { d =>
      val res = validateNoRoot(d).map(f).fold(es => Invalid(es), r => r.validate(d))
      res
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
    Rule(Path)(d => this.validate(d) orElse t.validate(d))

  def andThen[P](sub: => RuleLike[O, P]): Rule[I, P] =
    flatMap { o =>
      Rule[I, P](Path){ _ =>
        sub.validate(o)
      }
    }

  def andThen[P](m: Mapping[ValidationError, O, P]): Rule[I, P] =
    andThen(Rule.fromMapping(m))

  /**
    * This methods allows you to modify the Path of errors (if the result is a Invalid) when aplying the Rule
    */
  def repath(f: Path => Path): Rule[I, O] =
    Rule(f(path))(d => validateNoRoot(d))

  def map[B](f: O => B): Rule[I, B] =
    Rule(path)(d => this.validate(d).map(f))

  def ap[A](mf: Rule[I, O => A]): Rule[I, A] =
    Rule(Path) { d =>
      val a = validate(d)
      val f = mf.validateNoRoot(d)
      Validated.fromEither(
          (f *> a).toEither.right.flatMap(x => f.toEither.right.map(_ (x))))
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
    Rule(Path) { case (a, b) => f(a).validate(b) }

  def zero[O]: Rule[O, O] =
    toRule(RuleLike.zero[O])

  def pure[I, O](o: O): Rule[I, O] =
    Rule(Path)(_ => Valid(o))

  def apply[I, O](p: Path)(m: Mapping[(Path, Seq[ValidationError]), I, O]): Rule[I, O] =
    new Rule[I, O] {
      def path = p
      def validateNoRoot(data: I): VA[O] = m(data)
    }

  def lazily[I, O](path: Path)(r: Path => RuleLike[I, O]): Rule[I, O] =
    Rule(path) { i =>
      r(path).validateNoRoot(i)
    }

  def of[I, O](implicit r: Rule[I, O]): Rule[I, O] = r

  def toRule[I, O](r: RuleLike[I, O]): Rule[I, O] =
    new Rule[I, O] {
      def path = r.path
      def validateNoRoot(data: I): VA[O] = r.validateNoRoot(data)
    }

  def fromMapping[I, O](f: Mapping[ValidationError, I, O]): Rule[I, O] @@ Root =
    tag[Root](Rule[I, O](Path)(f(_: I).bimap(errs => Seq(Path -> errs), identity)))

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
          Rule[I, O](Path) { v =>
             (r1.validate(v) *> r2.validate(v)).bimap(
                 _.groupBy(_._1).map {
                   case (path, errs) =>
                     path -> errs.flatMap(_._2)
                 }.toSeq,
                 identity
             )
           }
        tag.apply(r)
      }
    }

  implicit def ruleCompose =
    new cats.arrow.Compose[Rule] {
      def compose[A, B, C](f: Rule[B,C], g: Rule[A,B]): Rule[A,C] =
        g andThen f
    }

  implicit def ruleFunctorSyntaxObs[I, O](
      r: Rule[I, O])(implicit fcb: SyntaxCombine[Rule[I, ?]])
    : FunctorSyntaxObs[Rule[I, ?], O] =
    new FunctorSyntaxObs[Rule[I, ?], O](r)(fcb)

  implicit def ruleHSequence0[I] =
    v3.HSequence0.applicativeHSequence0[Rule[I, ?]]
}

object Read {
  sealed trait Deferred[O] {
    def apply[I](i: I)(implicit r: RuleLike[I, O]) = r.validate(i)
  }

  def apply[O] = new Deferred[O]{}
}
