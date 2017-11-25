package jto.validation
package v3.tagless

import org.scalatest._
import cats.syntax.compose._
import cats.syntax.profunctor._

trait CrossCompile[T] extends WordSpec with Matchers {

  import types._

  val rg: Grammar[T, Rule]
  val wg: Grammar[T, op[Write]#λ]

  def upcast: wg.Out <:< rg.Out

  case class Info(label: String, email: Option[String], phones: Seq[String])

  val ex = Info("label", Option("fakecontact@gmail.com"), Seq("phone1", "phone2"))

  case class Contact(
    firstname: String,
    lastname: String,
    company: Option[String],
    informations: Seq[ContactInformation])

  case class ContactInformation(
    label: String,
    email: Option[String],
    phones: Seq[String])

  val expectedContact = Contact(
          "Julien",
          "Tournay",
          None,
          Seq(
            ContactInformation("Personal",
             Some("fakecontact@gmail.com"),
             List("01.23.45.67.89", "98.76.54.32.10"))))

  object NelExt {
    import cats.data._
    type Nel[A] = OneAnd[List, A]
    def nel[A](a: A, as: A*): Nel[A] = OneAnd[List, A](a, as.toList)

    trait ExtGrammar[K[_, _]] {
      type Out
      implicit def nel[A](implicit k: K[_ >: Out <: T, A]): K[T, Nel[A]]
    }

    object ExtRules extends ExtGrammar[Rule] {
      type Out = rg.Out
      implicit def nel[A](implicit k: Rule[_ >: Out <: T, A]): Rule[T, Nel[A]] =
        rg.list(k).map{ xs => OneAnd(xs.head, xs.tail) }
    }

    object ExtWrites extends ExtGrammar[op[Write]#λ] {
      type Out = wg.Out
      implicit def nel[A](implicit k: Write[A, _ >: Out <: T]): Write[Nel[A], T] =
        wg.list(k).contramap{ case OneAnd(h, t) => h :: t}
    }
  }

  "grammar" should {
      def info[K[_, _]](g: Grammar[T, K]) = {
        import g._
        at(Path \ "label").is(req[String] andThen notEmpty) ~:
        at(Path \ "email").is(opt(is[String] andThen email)) ~:
        at(Path \ "phones").is(req[Seq[String]] andThen forall(notEmpty)) ~:
        knil
      }

    "compile to symetric rule and write" in {
      def write = info[op[Write]#λ](wg).from[Info].rmap(upcast)
      def rule = info[Rule](rg).to[Info]
      def sym = (rule.validate _) compose (write.writes _)
      sym(ex) should === (Valid(ex))
    }

    "merge Ks" in {
      import cats.syntax.semigroup._
      val p = Path \ "percent"
      def percent[K[_, _]](g: Grammar[T, K]) = {
        import g._
        at(p).is(req[Int] andThen min(0) |+| max(100))
      }

      val write = percent[op[Write]#λ](wg).rmap(upcast)
      val rule = percent[Rule](rg)
      val sym = (rule.validate _) compose (write.writes _)

      sym(10) shouldBe Valid(10)
      sym(-10) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.min", 0))))
      sym(200) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.max", 100))))
    }

    "change path" in {
      val f = { (p: Path) =>
        val p2 =
          p.path.collect { case KeyPathNode(x) =>
            KeyPathNode(x.reverse)
          }
        Path(p2)
      }

      def write = info[op[Write]#λ](wg).from[Info].rmap(upcast)
      def writeInvert = info[op[Write]#λ](wg.mapPath(f)).from[Info].rmap(upcast)
      def rule = info[Rule](rg).to[Info]
      def ruleInvert = info[Rule](rg.mapPath(f)).to[Info]

      def sym = (ruleInvert.validate _) compose (writeInvert.writes _)
      sym(ex) should === (Valid(ex))

      def invert = (rule.validate _) compose (write.writes _)
      invert(ex) should === (Valid(ex))

      def invertKO = (rule.validate _) compose (writeInvert.writes _)
      invertKO(ex) should === (
        Invalid(List(
          (Path \ "label", List(ValidationError("error.required"))),
          (Path \ "phones", List(ValidationError("error.required")))
        )))

      def symKO = (ruleInvert.validate _) compose (write.writes _)
      symKO(ex) should === (
        Invalid(List(
          (Path \ "lebal", List(ValidationError("error.required"))),
          (Path \ "senohp", List(ValidationError("error.required")))
        )))
    }

    "support complex use cases" in {
      implicit def info[K[_, _]](implicit g: Grammar[T, K]) = {
        import g._
        as[ContactInformation].from {
          at(Path \ "label").is(req[String] andThen notEmpty) ~:
          at(Path \ "email").is(opt(is[String] andThen email)) ~:
          at(Path \ "phones").is(req[Seq[String]] andThen forall(notEmpty)) ~:
          knil
        }
      }

      def contact[K[_, _]](implicit g: Grammar[T, K]) = {
        import g._
        as[Contact].from {
          at(Path \ "firstname").is(req[String] andThen notEmpty) ~:
          at(Path \ "lastname").is(req[String] andThen notEmpty) ~:
          at(Path \ "company").is(opt[String]) ~:
          at(Path \ "contacts").is(req(seq(info(g)))) ~:
          knil
        }
      }

      val write = contact[op[Write]#λ](wg).rmap(upcast)
      val rule = contact[Rule](rg)

      val sym = (rule.validate _) compose (write.writes _)
      sym(expectedContact) should === (Valid(expectedContact))
    }

    "Support extension" in {

      import shapeless.tag, tag.@@
      trait GrammarExtensions[K[_, _]] {
        def startsWith(s: String): K[String, String] @@ Root
      }

      object W extends GrammarExtensions[op[Write]#λ] {
        def startsWith(s: String) = Write.zero[String]
      }

      object R extends GrammarExtensions[Rule] {
        def startsWith(s: String) =
          Rule.fromMapping {
            case x if x.startsWith(s) => Valid(x)
            case _ => Invalid(Seq(ValidationError("error.startsWith")))
          }
      }

      def info[K[_, _]](implicit g: Grammar[T, K], e: GrammarExtensions[K]) = {
        import g._, e._
        import cats.syntax.semigroup._
        as[ContactInformation].from {
          at(Path \ "label").is(req[String] andThen (notEmpty |+| startsWith("Per"))) ~:
          at(Path \ "email").is(opt(is[String] andThen email)) ~:
          at(Path \ "phones").is(req[Seq[String]] andThen forall(notEmpty)) ~:
          knil
        }
      }

      val write = info[op[Write]#λ](wg, W).rmap(upcast)
      val rule = info[Rule](rg, R)

      val sym = (rule.validate _) compose (write.writes _)
      val e = expectedContact.informations.head
      sym(e) should === (Valid(e))
    }

    "Automatically derive structure definitions" should {

      "support simple cases" in {
        case class Case1(i: Int)
        val case1 = Case1(1)
        val auto1 = Auto[Case1]
        val rule1 = auto1(rg)
        val write1 = auto1[T, op[Write]#λ, wg.Out](wg).rmap(upcast)
        val sym1 = (rule1.validate _) compose (write1.writes _)
        sym1(case1) should === (Valid(case1))


        case class Case2(i: Int, s: String)
        val case2 = Case2(1, "foo")
        val auto2 = Auto[Case2]
        val rule2 = auto2(rg)
        val write2 = auto2[T, op[Write]#λ, wg.Out](wg).rmap(upcast)
        val sym2 = (rule2.validate _) compose (write2.writes _)
        sym2(case2) should === (Valid(case2))
      }

      "support simple cases using extended grammar" in {
        import NelExt._
        case class CaseExtended(i: Int, s: Option[String], nel: Nel[Int])
        val caseExt = CaseExtended(1, Option("foo"), nel(1, 2))
        val autoExt = Auto[CaseExtended]
        val ruleExt = autoExt(rg, ExtRules)
        // TODO: improve type inference
        val writeExt = autoExt[T, op[Write]#λ, wg.Out](wg, ExtWrites).rmap(upcast)
        val symExt = (ruleExt.validate _) compose (writeExt.writes _)
        symExt(caseExt) should === (Valid(caseExt))
      }

      "support complex cases using extended grammar" in {
        import NelExt._
        case class ContactNel(
          firstname: String,
          lastname: String,
          company: Option[String],
          informations: Nel[ContactInformationNel])

        case class ContactInformationNel(
          label: String,
          email: Option[String],
          phones: Nel[String])

         val info =
            ContactInformationNel("Personal",
             Option("fakecontact@gmail.com"),
             nel("01.23.45.67.89", "98.76.54.32.10"))

        val auto = Auto[ContactInformationNel]
        val rule = auto(rg, ExtRules)
        val write = auto[T, op[Write]#λ, wg.Out](wg, ExtWrites).rmap(upcast)
        val sym = (rule.validate _) compose (write.writes _)
        sym(info) should === (Valid(info))
      }

    }
  }

}
