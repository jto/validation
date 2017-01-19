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

  val header = "// $COVERAGE-OFF$Disabling coverage for auto-generated boilerplate"

  val minArity = 2
  val maxArity = 22

  val templates: Seq[Template] = List(
    InvariantSyntax,
    FunctorSyntax,
    ContravariantSyntax,
    UFreeSyntax,
    LiftInstances
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
        -    def apply[B](f1: ${`(A..N)`} => B, f2: B => ${`(A..N)`})(implicit fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        combine(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
        -        (b: B) => { val (${`a..n`}) = f2(b); ${`new ~(.., n)`} }
        -      )
        -
        -    def as[B](implicit t: CaseClassTupler.Aux[B, ${`(A..N)`}], fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        combine(m1, m2))({ case ${`a~n`} => t.from(${`(a..n)`}) })(
        -        (b: B) => { val (${`a..n`}) = t.to(b); ${`new ~(.., n)`} }
        -      )
        -
        -    def tupled(implicit fu: Invariant[M]): M[${`(A..N)`}] =
        -      apply[${`(A..N)`}]({ (${`a:A..n:N`}) => (${`a..n`}) }, { (a: ${`(A..N)`}) => (${`a._1..a._N`}) })
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
        -    def apply[B](f: ${`(A..N)`} => B)(implicit fu: Functor[M]): M[B] =
        -      fu.map[${`A~N`}, B](combine(m1, m2))({ case ${`a~n`} => f(${`a..n` }) })
        -
        -    def as[B](implicit t: CaseClassTupler.Aux[B, ${`(A..N)`}], fu: Functor[M]): M[B] =
        -      fu.map[${`A~N`}, B](combine(m1, m2))({ case ${`a~n`} => t.from(${`(a..n)`}) })
        -
        -    def tupled(implicit fu: Functor[M]): M[${`(A..N)`}] =
        -      apply[${`(A..N)`}]({ (${`a:A..n:N`}) => (${`a..n`}) })
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
        -    def apply[B](f: B => ${`(A..N)`})(implicit fu: Contravariant[M]): M[B] =
        -      fu.contramap[${`A~N`}, B](combine(m1, m2))((b: B) => { val (${`a..n`}) = f(b); ${`new ~(.., n)`} })
        -
        -    def as[B](implicit t: CaseClassTupler.Aux[B, ${`(A..N)`}], fu: Contravariant[M]): M[B] =
        -      fu.contramap[${`A~N`}, B](combine(m1, m2))((b: B) => { val (${`a..n`}) = t.to(b); ${`new ~(.., n)`} })
        -
        -    def tupled(implicit fu: Contravariant[M]): M[${`(A..N)`}] =
        -      apply[${`(A..N)`}]({ (a: ${`(A..N)`}) => (${`a._1..a._N`}) })
        -  }
        -
        |}
      """
    }
  }

  object UFreeSyntax extends Template {
    def filename(root: File) = root / "UFreeSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val `((A..N))` = synTypes.reduce[String] { case (acc, el) => s"($acc, $el)" }
      val `((a..n))` = `((A..N))`.toLowerCase

      val next = if (arity >= maxArity) "" else
        s"""
        |-    def ~[IX <: HList, IO <: HList, X]
        |-      (fb: UFree.Aux[X, IX])
        |-      (implicit p: UnliftPrepend.Aux[Implicits, IX, IO])
        |-      : UFreeSyntax${arity + 1}[IO, ${`A..N`}, X] =
        |-        UFreeSyntax${arity + 1}(UFree.Zip(fa, fb)(p): UFree.Aux[(${`((A..N))`}, X), IO])
        """.trim.stripMargin

      block"""
        |package jto.validation
        |package free
        |
        |object UFreeSyntax {
        -  case class UFreeSyntax${arity}[Implicits <: HList, ${`A..N`}](fa: UFree.Aux[(${`((A..N))`}), Implicits]) {
        -    def imap[X](f: ${`(A..N)`} => X, g: X => (${`(A..N)`})): UFree.Aux[X, Implicits] =
        -      UFree.Imap(fa)(
        -        { case ${`((a..n))`} => f(${`a..n`}) },
        -        { x =>
        -          val ${`(a..n)`} = g(x)
        -          ${`((a..n))`}
        -        })
        -
        -    def as[X](implicit t: CaseClassTupler.Aux[X, ${`(A..N)`}]): UFree.Aux[X, Implicits] =
        -      UFree.Imap(fa)(
        -        { case ${`((a..n))`} => t.from(${`(a..n)`}) },
        -        { x =>
        -          val ${`(a..n)`} = t.to(x)
        -          ${`((a..n))`}
        -        })
        -
        $next
        -  }
        -
        |}
      """
    }
  }

  object LiftInstances extends Template {
    def filename(root: File) = root / "LiftInstances.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val `A::N` = synTypes.mkString(" :: ")
      val `a::n` = `A::N`.toLowerCase
      val `P=>F[A]..P=>F[N]` = synTypes.map(t => s"(Path=>F[$t])").mkString(" :: ")
      val `a:P=>F[A]..n:P=>F[N]` = synVals.zip(synTypes).map { case (a, t) => s"$a: Path=>F[$t]"}.mkString(", ")

      block"""
        |package jto.validation
        |package free
        |
        |import cats._
        |
        |trait LiftInstances {
        |  implicit class liftHList1[A, A0](self: UFree.Aux[A, A0 :: HNil]) {
        |    def materialize[F[_]: InvariantMonoidal](implicit a0: Path=>F[A0]): F[A] =
        |      self.mat(
        |        new jto.validation.free.Lift[λ[t => Path=>F[t]], A0 :: HNil] {
        |          type Out = (Path=>F[A0]) :: HNil
        |          def instances = a0 :: HNil
        |        }
        |      )
        |  }
        |
        -  implicit class liftHList${arity}[A, ${`A..N`}](self: UFree.Aux[A, ${`A::N`} :: HNil]) {
        -    def materialize[F[_]: InvariantMonoidal](implicit ${`a:P=>F[A]..n:P=>F[N]`}): F[A] =
        -      self.mat(
        -        new jto.validation.free.Lift[λ[t => Path=>F[t]], ${`A::N`} :: HNil] {
        -          type Out = ${`P=>F[A]..P=>F[N]`} :: HNil
        -          def instances = ${`a::n`} :: HNil
        -        }
        -      )
        -  }
        -
        |}
      """
    }
  }
}
