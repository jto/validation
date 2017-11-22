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

      val expected = Contact(
          "Julien",
          "Tournay",
          None,
          Seq(
            ContactInformation("Personal",
             Some("fakecontact@gmail.com"),
             List("01.23.45.67.89", "98.76.54.32.10"))))

      val write =
        contact[op[Write]#λ](wg).rmap(upcast)
      val rule =
        contact[Rule](rg)

      val sym = (rule.validate _) compose (write.writes _)
      sym(expected) should === (Valid(expected))
    }

    "Automatically derive structure definitions" in {
      // def info = auto[ContactInformation]
        // new DerivedK {
        //   def apply[K[_, _]](implicit g: Grammar[T, K]) = {
        //     import g._
        //     ...
        //   }
        // }
    }
  }

}
