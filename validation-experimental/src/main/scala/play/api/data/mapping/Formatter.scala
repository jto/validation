package play.api.data.mapping

import shapeless._
import ops.record.{ Selector => RSelector, Updater }
import record.{ FieldType }

trait Get[I, O] {
  outer =>
  val path: Path
  val lens: Lens[I, O]

  def read(sub: => RuleLike[O, O]): Rule[I, I] = Rule { i =>
    Rule.toRule(sub).repath(path ++ _)
      .fmap(_ => i)
      .validate(lens.get(i))
  }
  // def read[I, O](implicit r: Path => RuleLike[I, O]): Rule[I, O] =

  def \[Out0 <: HList : lens.Gen, V](k: Witness)(implicit s: RSelector.Aux[Out0, k.T, V], u: Updater.Aux[Out0, FieldType[k.T, V], Out0]) =
    new Get[I, V]{
      val nodeName = k match {
        case w: Witness.Aux[Symbol] => w.value.name
        case _ => k.value.toString
      }
      val path = outer.path \ nodeName
      val lens = outer.lens >> k
    }

  def apply(f: Get[I, I] => Rule[I, O]): Rule[I, O] = Rule.toRule(f(Get[I]))
}

object Get {
  def lens[I, O](l: Lens[I, O]): Get[I, O] = new Get[I, O] {
    type Out = O
    val path = Path
    val lens = l
  }

  def apply[I]: Get[I, I] = lens(shapeless.lens.apply[I])
}