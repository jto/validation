package jto.validation
package free

object UFreeMacro {
  import scala.reflect.macros.whitebox.Context

  def free[X: c.WeakTypeTag, Implicits](c: Context)(): c.Tree = {
    import c.universe._

    val helper = new { val context: c.type = c } with MappingMacros.Helper
    import helper._

    val (_, constructorParamss) = getConstructorParamss[X]

    val leafs: List[Tree] =
      for { g <- constructorParamss.headOption.toList; param <- g } yield {
        val term = param.asTerm
        val name = q"""${term.name.toString}"""
        q"""(_root_.jto.validation.Path \ $name).as[${term.typeSignature}]"""
      }

    val body = leafs.reduce((t1, t2) => q"$t1 ~ $t2")
    val typeX = weakTypeOf[X].dealias
    q"""$body.as[$typeX]"""
  }
}
