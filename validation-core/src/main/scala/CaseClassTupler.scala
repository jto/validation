package jto.validation

/**
 * Lightweight alternative to `shapeless.Generic` limited to products and targeting `TupleN`s.
 */
trait CaseClassTupler[T] {
  type TupleRepr

  def to(t: T): TupleRepr
  def from(t: TupleRepr): T
}

object CaseClassTupler {
  type Aux[T, R] = CaseClassTupler[T] { type TupleRepr = R }
  def apply[T](implicit t: CaseClassTupler[T]): Aux[T, t.TupleRepr] = t

  implicit def materialize[T, R]: Aux[T, R] = macro CaseClassTuplerMacro.mkCaseClassTupler[T, R]
}

object CaseClassTuplerMacro {
  import scala.reflect.macros.whitebox.Context

  def mkCaseClassTupler[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val tpe: Type = weakTypeOf[T]
    val helper = new { val context: c.type = c } with MappingMacros.Helper
    import helper._

    val (apply, unapply) = lookup[T]
    val (_, constructorParamss) = getConstructorParamss[T]
    val isArityOne: Boolean = constructorParamss.headOption.toList.flatten.size == 1

    val TypeRef(_, _, ps) = unapply.returnType
    val tupleRepr = tq"${ps.head}"

    val to = q"""_root_.scala.Function.unlift($unapply _)(c)"""
    val from = if (isArityOne) q"""$apply(t)""" else q"""($apply _).tupled(t)"""

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      {
        final class $clsName extends _root_.jto.validation.CaseClassTupler[$tpe] {
          type TupleRepr = $tupleRepr
          def to(c: $tpe): TupleRepr   = $to
          def from(t: TupleRepr): $tpe = $from
        }
        new $clsName(): _root_.jto.validation.CaseClassTupler.Aux[$tpe, $tupleRepr]
      }
    """
  }
}
