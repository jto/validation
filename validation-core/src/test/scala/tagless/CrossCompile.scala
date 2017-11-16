package jto.validation
package v3.tagless

import org.scalatest._
import cats.syntax.compose._

trait CrossCompile[T] extends WordSpec with Matchers {

  import types._

  val rg: Grammar[T, Rule]
  val wg: Grammar[T, flip[Write]#λ]

  def upcast: wg.Out <:< rg.Out

  "grammar" should {

    "compile to symetric rule and write" in {
      case class Info(label: String, email: Option[String], phones: Seq[String])
      val ex = Info("label", Option("fakecontact@gmail.com"), Seq("phone1", "phone2"))

      def info[K[_, _]](g: Grammar[T, K]) = {
        import g._
        at(Path \ "label").is(req[String] andThen notEmpty) ~:
        at(Path \ "email").is(opt(is[String] andThen email)) ~:
        at(Path \ "phones").is(req[Seq[String]] andThen forall(notEmpty)) ~:
        knil
      }

      import cats.syntax.profunctor._
      def write = info[flip[Write]#λ](wg).from[Info].rmap(upcast)
      def rule = info[Rule](rg).to[Info]
      def sym = (rule.validate _) compose (write.writes _)
      sym(ex) should === (Valid(ex))
    }

    // "compose" in {
    //   case class Id(value: String)
    //   implicit val idW: Write[Id, String] = Write(id => id.value)
    //   implicit val idR: Rule[String, Id] = Rule.zero[String].map(Id.apply)

    //   def id[K[_, _]](implicit g: Grammar[JsValue, K], valid: K[String, Id]) = {
    //     import g._
    //     at(Path \ "id")(req[String] andThen valid)
    //   }

    //   val ex = Id("value")

    //   val write = id[flip[Write]#λ]
    //   val rule = id[Rule]
    //   val sym = (rule.validate _) compose (write.writes _)
    //   sym(ex) shouldBe Valid(ex)

    // }

    // "merge Ks" in {
    //   import cats.syntax.semigroup._
    //   val p = Path \ "percent"
    //   def percent[K[_, _]](implicit g: Grammar[JsValue, K]) = {
    //     import g._
    //     at(p)(req[Int] andThen min(0) |+| max(100))
    //   }

    //   val write = percent[flip[Write]#λ]
    //   val rule = percent[Rule]
    //   val sym = (rule.validate _) compose (write.writes _)

    //   sym(10) shouldBe Valid(10)
    //   sym(-10) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.min", 0))))
    //   sym(200) shouldBe Invalid(Seq((p) -> Seq(ValidationError("error.max", 100))))
    // }

    // TODO: PORT
    // "change path" in {
    //   def info[K[_, _]](g: Grammar[JsValue, K]) = {
    //     import g._
    //     at(Path \ "label")(req[String] andThen notEmpty) ~:
    //     at(Path \ "email")(opt(is[String] andThen email)) ~:
    //     at(Path \ "phones")(req[Seq[String]] andThen forall(notEmpty)) ~:
    //     knil
    //   }

    //   val f = { (p: Path) =>
    //     val p2 =
    //       p.path.collect { case KeyPathNode(x) =>
    //         KeyPathNode(x.reverse)
    //       }
    //     Path(p2)
    //   }

    //   val write = info(WritesGrammar.mapPath(f)).from[Info]
    //   val rule = info(RulesGrammar.mapPath(f)).to[Info]
    //   val json = write.writes(ex)
    //   json shouldBe Json.parse("""{"lebal":"label","liame":"fakecontact@gmail.com","senohp":["phone1","phone2"]}""")
    //   rule.validate(json) shouldBe Valid(ex)
    // }
  }

}
