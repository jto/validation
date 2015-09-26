package jto.validation

import shapeless._
import ops.record.{ Selector => RSelector, Updater }
import labelled.FieldType

trait Get[I, O] {
  outer =>
  def path: Path
  def lens: Lens[I, O]

  def read(sub: => RuleLike[O, O]): Rule[I, I] = Rule { i =>
    Rule.toRule(sub)
      .repath(path ++ _)
      .map(_ => i)
      .validate(lens.get(i))
  }

  def \[Out0 <: HList, V](k: Witness)
    (implicit
      s: RSelector.Aux[Out0, k.T, V],
      u: Updater.Aux[Out0, FieldType[k.T, V], Out0],
      mkLens: MkFieldLens[O, k.T]
    ) =
    new Get[I, mkLens.Elem] {
      val nodeName = k.value match {
        case s: Symbol => s.name
        case s => s.toString
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
