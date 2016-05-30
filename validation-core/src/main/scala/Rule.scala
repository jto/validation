package jto.validation

import cats.Applicative

trait Rule[I, O] {

  /**
    * Apply the Rule to `data`
    * @param data The data to validate
    * @return The Result of validating the data
    */
  def validate(data: I): VA[O]
}

object Rule {
  implicit def zero[O]: Rule[O, O] = Rule[O, O](Valid.apply)
}

trait Rule[I, O] extends Rule[I, O] {

  @deprecated("use andThen instead.", "2.0")
  def compose[P](path: Path)(sub: => Rule[O, P]): Rule[I, P] =
    andThen(path)(sub)

  /**
    * Compose two Rules
    * {{{
    *   val r1: Rule[JsValue, String] = // implementation
    *   val r2: Rule[String, Date] = // implementation
    *   val r = r1 .andThen(r2)
    *
    * }}}
    * @param path a prefix for the errors path if the result is a `Invalid`
    * @param sub the second Rule to apply
    * @return The combination of the two Rules
    */
  def andThen[P](path: Path)(sub: => Rule[O, P]): Rule[I, P] =
    this.flatMap { o =>
      Rule(_ => sub.validate(o))
    }.repath(path ++ _)

  def flatMap[B](f: O => Rule[I, B]): Rule[I, B] =
    Rule { d =>
      this.validate(d).map(f).fold(es => Invalid(es), r => r.validate(d))
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
  def orElse[OO >: O](t: => Rule[I, OO]): Rule[I, OO] =
    Rule(d => this.validate(d) orElse t.validate(d))

  @deprecated("use andThen instead.", "2.0")
  def compose[P](sub: => Rule[O, P]): Rule[I, P] = andThen(sub)
  def andThen[P](sub: => Rule[O, P]): Rule[I, P] = andThen(Path)(sub)

  @deprecated("use andThen instead.", "2.0")
  def compose[P](m: Mapping[ValidationError, O, P]): Rule[I, P] = andThen(m)
  def andThen[P](m: Mapping[ValidationError, O, P]): Rule[I, P] =
    andThen(Rule.fromMapping(m))

  /**
    * Create a new Rule the validate `this` Rule and `r2` simultaneously
    * If `this` and `r2` both fail, all the error are returned
    * {{{
    *   val valid = Json.obj(
    *      "firstname" -> "Julien",
    *      "lastname" -> "Tournay")
    *   val composed = notEmpty |+| minLength(3)
    *   (Path \ "firstname").read(composed).validate(valid) // Valid("Julien")
    *  }}}
    */
  def |+|[OO <: O](r2: Rule[I, OO]): Rule[I, O] =
    Rule[I, O] { v =>
      (this.validate(v) *> r2.validate(v)).bimap(
          _.groupBy(_._1).map {
            case (path, errs) =>
              path -> errs.flatMap(_._2)
          }.toSeq,
          identity
      )
    }

  /**
    * This methods allows you to modify the Path of errors (if the result is a Invalid) when aplying the Rule
    */
  def repath(f: Path => Path): Rule[I, O] =
    Rule(
        d =>
          this
            .validate(d)
            .bimap(_.map { case (p, errs) => f(p) -> errs }, identity))

  def map[B](f: O => B): Rule[I, B] =
    Rule(d => this.validate(d).map(f))

  @deprecated("fmap is deprecated, use map instead", "2.0")
  def fmap[B](f: O => B): Rule[I, B] = map(f)

  def ap[A](mf: Rule[I, O => A]): Rule[I, A] =
    Rule { d =>
      val a = validate(d)
      val f = mf.validate(d)
      Validated.fromEither(
          (f *> a).toEither.right.flatMap(x => f.toEither.right.map(_ (x))))
    }
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
    Rule { case (a, b) => f(a).validate(b) }

  def zero[O]: Rule[O, O] =
    toRule(Rule.zero[O])

  def pure[I, O](o: O): Rule[I, O] =
    Rule(_ => Valid(o))

  def apply[I, O](m: Mapping[(Path, Seq[ValidationError]), I, O]): Rule[I, O] =
    new Rule[I, O] {
      def validate(data: I): VA[O] = m(data)
    }

  def of[I, O](implicit r: Rule[I, O]): Rule[I, O] = r

  def toRule[I, O](r: Rule[I, O]): Rule[I, O] =
    new Rule[I, O] {
      def validate(data: I): VA[O] = r.validate(data)
    }

  def fromMapping[I, O](f: Mapping[ValidationError, I, O]): Rule[I, O] =
    Rule[I, O](f(_: I).bimap(errs => Seq(Path -> errs), identity))

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

  implicit def ruleFunctorSyntaxObs[I, O](
      r: Rule[I, O])(implicit fcb: SyntaxCombine[Rule[I, ?]])
    : FunctorSyntaxObs[Rule[I, ?], O] =
    new FunctorSyntaxObs[Rule[I, ?], O](r)(fcb)
}
