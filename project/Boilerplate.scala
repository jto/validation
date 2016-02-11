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

  /** Returns a seq of the generated files. As a side-effect, it actually generates them... */
  def gen(dir: File) = {
    val template = FunctionalBuilder
    val tgtFile = template.filename(dir)
    IO.write(tgtFile, template.body)
    Seq(tgtFile)
  }

  class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => s"A$n")
    val synVals      = (0 until arity) map (n => s"a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v,t) => v + ": " + t}
    val `A..N`       = synTypes.mkString(", ")
    val `a..n`       = synVals.mkString(", ")
    val `_.._`       = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "
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

  object FunctionalBuilder extends Template {
    def filename(root: File) = root /  "jto" / "validation" / "FunctionalBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val `a~n`          = synVals.mkString(" ~ ")
      val `A~N`          = synTypes.mkString(" ~ ")
      val `A~N-1`        = (0 until arity - 1).map(n => s"A$n").mkString(" ~ ")
      val `a._1..a._N`   = (1 to arity) map (n => s"a._$n") mkString ", "
      val `new ~(.., n)` = synVals.reduce[String] { case (acc, el) => s"new ~($acc, $el)" }

      val next = if (arity + 1 <= maxArity)
        s"def ~[A$arity](m3: M[A$arity]) = new CanBuild${arity+1}[${`A..N`}, A$arity](canBuild(m1, m2), m3)"
      else
        ""

      val and = if (arity + 1 <= maxArity)
        s"def and[A$arity](m3: M[A$arity]) = this.~(m3)"
      else
        ""

      block"""
        |package jto.validation
        |
        |import cats.Functor
        |import cats.functor._
        |
        |case class ~[A, B](_1: A, _2: B)
        |
        |trait FunctionalCanBuild[M[_]] {
        |  def apply[A, B](ma: M[A], mb: M[B]): M[A ~ B]
        |}
        |
        |class FunctionalBuilderOps[M[_], A](ma: M[A])(implicit fcb: FunctionalCanBuild[M]) {
        |  def ~[B](mb: M[B]): FunctionalBuilder[M]#CanBuild2[A, B] = {
        |    val b = new FunctionalBuilder(fcb)
        |    new b.CanBuild2[A, B](ma, mb)
        |  }
        |}
        |
        |class FunctionalBuilder[M[_]](canBuild: FunctionalCanBuild[M]) {
        |
        -  class CanBuild$arity[${`A..N`}](m1: M[${`A~N-1`}], m2: M[A${arity-1}]) {
        -    $next
        -
        -    $and
        -
        -    def apply[B](f: (${`A..N`}) => B)(implicit fu: Functor[M]): M[B] =
        -      fu.map[${`A~N`}, B](canBuild(m1, m2))({ case ${`a~n`} => f(${`a..n`}) })
        -
        -    def apply[B](f: B => (${`A..N`}))(implicit fu: Contravariant[M], d: DummyImplicit): M[B] =
        -      fu.contramap(canBuild(m1, m2))((b: B) => { val (${`a..n`}) = f(b); ${`new ~(.., n)`} })
        -
        -    def unlifted[B](f: B => Option[(${`A..N`})])(implicit fu: Contravariant[M]): M[B] =
        -      fu.contramap(canBuild(m1, m2))((b: B) => { val (${`a..n`}) = f(b).get; ${`new ~(.., n)`} })
        -
        -    def apply[B](f1: (${`A..N`}) => B, f2: B => (${`A..N`}))(implicit fu: Invariant[M], d: DummyImplicit): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        canBuild(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
        -        (b: B) => { val (${`a..n`}) = f2(b); ${`new ~(.., n)`} }
        -      )
        -
        -    def unlifted[B](f1: (${`A..N`}) => B, f2: B => Option[(${`A..N`})])(implicit fu: Invariant[M]): M[B] =
        -      fu.imap[${`A~N`}, B](
        -        canBuild(m1, m2))({ case ${`a~n`} => f1(${`a..n`}) })(
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
}
