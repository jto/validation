import sbt._

/**
 * Copied, with some modifications, from https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala
 *
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 */

object Boilerplate {
  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }

  val header = "// Auto-generated boilerplate"

  val minArity = 2
  val maxArity = 22

  val templates: Seq[Template] = List(
    InvariantSyntax,
    FunctorSyntax,
    ContravariantSyntax,
    GenCartesianBuilders
  )

  /** Returns a seq of the generated files. As a side-effect, it actually generates them... */
  def gen(dir: File) =
    for(template <- templates) yield {
      val tgtFile = template.filename(dir / "jto" / "validation")
      IO.write(tgtFile, template.body)
      tgtFile
    }

  class TemplateVals(val arity: Int) {
    val synTypes       = (0 until arity) map (n => s"A$n")
    val synVals        = (0 until arity) map (n => s"a$n")
    val synTypedVals   = (synVals zip synTypes) map { case (v,t) => v + ": " + t}
    val `A..N`         = synTypes.mkString(", ")
    val `a..n`         = synVals.mkString(", ")
    val `_.._`         = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`       = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`       = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`       = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`     = synTypedVals mkString ", "
    val `a~n`          = synVals.mkString(" ~ ")
    val `A~N`          = synTypes.mkString(" ~ ")
    val `A~N-1`        = (0 until arity - 1).map(n => s"A$n").mkString(" ~ ")
    val `a._1..a._N`   = (1 to arity) map (n => s"a._$n") mkString ", "
    val `new ~(.., n)` = synVals.reduce[String] { case (acc, el) => s"new ~($acc, $el)" }
  }

  trait Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def range = minArity to maxArity
    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */

  object InvariantSyntax extends Template {
    def filename(root: File) = root / "InvariantSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new InvariantSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.functor.Invariant
        |
        |class InvariantSyntax[M[_]](combine: SyntaxCombine[M]) {
        |
        -  class InvariantSyntax$arity[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f1: (${`A..N`}) => B, f2: B => (${`A..N`}))(implicit fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        combine(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
        -        (b: B) => { val (${`a..n`}) = f2(b); ${`new ~(.., n)`} }
        -      )
        -
        -    def unlifted[B](f1: (${`A..N`}) => B, f2: B => Option[(${`A..N`})])(implicit fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        combine(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
        -        (b: B) => { val (${`a..n`}) = f2(b).get; ${`new ~(.., n)`} }
        -      )
        -
        -    def tupled(implicit fu: Invariant[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (${`a:A..n:N`}) => (${`a..n`}) }, { (a: (${`A..N`})) => (${`a._1..a._N`}) })
        -  }
        -
        |}
      """
    }
  }

  object FunctorSyntax extends Template {
    def filename(root: File) = root / "FunctorSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new FunctorSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.Functor
        |
        |class FunctorSyntax[M[_]](combine: SyntaxCombine[M]) {
        |
        -  class FunctorSyntax${arity}[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f: (${`A..N`}) => B)(implicit fu: Functor[M]): M[B] =
        -      fu.map[${`A~N`}, B](combine(m1, m2))({ case ${`a~n`} => f(${`a..n`}) })
        -
        -    def tupled(implicit fu: Functor[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (${`a:A..n:N`}) => (${`a..n`}) })
        -  }
        -
        |}
      """
    }
  }

  object ContravariantSyntax extends Template {
    def filename(root: File) = root / "ContravariantSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val next = if (arity >= maxArity) "" else
        s"def ~[A$arity](m3: M[A$arity]) = new ContravariantSyntax${arity+1}[${`A..N`}, A$arity](combine(m1, m2), m3)"

      block"""
        |package jto.validation
        |
        |import cats.functor.Contravariant
        |
        |class ContravariantSyntax[M[_]](combine: SyntaxCombine[M]) {
        |
        -  class ContravariantSyntax${arity}[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    def apply[B](f: B => (${`A..N`}))(implicit fu: Contravariant[M]): M[B] =
        -      fu.contramap(combine(m1, m2))((b: B) => { val (${`a..n`}) = f(b); ${`new ~(.., n)`} })
        -
        -    def unlifted[B](f: B => Option[(${`A..N`})])(implicit fu: Contravariant[M]): M[B] =
        -      fu.contramap(combine(m1, m2))((b: B) => { val (${`a..n`}) = f(b).get; ${`new ~(.., n)`} })
        -
        -    def tupled(implicit fu: Contravariant[M]): M[(${`A..N`})] =
        -      apply[(${`A..N`})]({ (a: (${`A..N`})) => (${`a._1..a._N`}) })
        -  }
        -
        |}
      """
    }
  }

  object GenCartesianBuilders extends Template {
    def filename(root: File) = root / "CartesianBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val tpesString = synTypes mkString ", "
      val params = (synVals zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "
      val next = if (arity + 1 <= maxArity) {
        s"def ~[Z](z: F[Z]) = new CartesianBuilder${arity + 1}(${`a..n`}, z)"
      } else {
        ""
      }

      val n = if (arity == 1) { "" } else { arity.toString }

      val meVals = (0 until arity) map (n => s"c$n")
      val meParams = (meVals zip synTypes) map { case (v, t) => s"$v: Case1[f.type, $t]"} mkString ", "
      val meBody = (synVals zip meVals).map { case (v, m) => s"functor.map($v)(a => $m(a))" } mkString " ~ "

      val asVals = (0 until arity) map (n => s"u$n")
      val asParams = (asVals zip synTypes) map { case (u, t) => s"$u: Unify[G, $t]"} mkString ", "
      val asBody = (synVals zip asVals).map { case (v, u) => s"functor.map($v)(a => $u(a))" } mkString " ~ "

      val as =
        s"def as[G[_]](implicit functor: Functor[F], $asParams) = new CartesianBuilder[λ[α => F[G[α]]]] ~ $asBody"

      val mapEach =
        s"def mapEach(f: Poly)(implicit functor: Functor[F], $meParams) = new CartesianBuilder[F] ~ $meBody"

      val map =
        if (arity == 1) s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F]): F[Z] = functor.map(${`a..n`})(f)"
        else s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F], cartesian: Cartesian[F]): F[Z] = Cartesian.map$n(${`a..n`})(f)"

      val contramap =
        if (arity == 1) s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap(${`a..n`})(f)"
        else s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.contramap$n(${`a..n`})(f)"

      val imap =
        if (arity == 1) s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F]): F[Z] = invariant.imap(${`a..n`})(f)(g)"
        else s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.imap$n(${`a..n`})(f)(g)"

      val tupled = if (arity != 1) {
        s"def tupled(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[(${`A..N`})] = Cartesian.tuple$n(${`a..n`})"
      } else {
        ""
      }

      block"""
        |package jto.validation
        |
        |import cats.functor.{ Contravariant, Invariant }
        |import cats.{ Functor, Cartesian, Apply }
        |import shapeless.Poly
        |import shapeless.PolyDefns._
        |
        |private[validation] final class CartesianBuilder[F[_]] {
        |  def ~[A](a: F[A]) = new CartesianBuilder1(a)
        |
        |  private[validation] final class CartesianBuilder1[A0](a0:F[A0]) {
        |    def ~[Z](z: F[Z]) = new CartesianBuilder2(a0, z)
        |    def apWith[Z](f: F[(A0) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap(f)(a0)
        |    def map[Z](f: (A0) => Z)(implicit functor: Functor[F]): F[Z] = functor.map(a0)(f)
        |    def contramap[Z](f: Z => (A0))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap(a0)(f)
        |    def imap[Z](f: (A0) => Z)(g: Z => (A0))(implicit invariant: Invariant[F]): F[Z] = invariant.imap(a0)(f)(g)
        |    def mapEach(f: Poly)(implicit functor: Functor[F], c0: Case1[f.type, A0]) =
        |     new CartesianBuilder[F] ~ functor.map(a0)(a => c0(a))
        |    //def as[G[_]](implicit functor: Functor[F], u0: Unify[G, A0]) = new CartesianBuilder[λ[α => F[G[α]]]] ~ functor.map(a0)(a => u0(a))
        | }
        |
        -  private[validation] final class CartesianBuilder$arity[${`A..N`}]($params) {
        -    $next
        -    def apWith[Z](f: F[(${`A..N`}) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap$n(f)(${`a..n`})
        -    $map
        -    $contramap
        -    $imap
        -    $tupled
        -    $mapEach
        - }
        |}
      """
    }
  }
}
