package vorm.persisted

import language.experimental.macros
import scala.reflect.makro.Context
import scala.reflect.api.Modifier._

object PersistedEnabler {

  def toPersisted[T](instance: T, id: Long): T with Persisted = macro impl[T]

  def impl[T: c.TypeTag](c: Context)(instance: c.Expr[T], id: c.Expr[Long]) = {
    import c.mirror._

    val t = c.typeTag[T]
    val u = t.tpe.typeSymbol
    if (!(u.modifiers contains `case`))
      c.abort(c.enclosingPosition, "toPersisted only accepts case classes, you provided %s".format(t.tpe))

    // how did I know what trees to generate?
    // read up the docs at http://scalamacros.org/documentation.html
    val fields = u.typeSignature.members.filter(m => m.owner == u && !m.isMethod).toList.reverse // members are returned in reverse order
    var vals = fields map (field => ValDef(Modifiers(Set(`override`, paramAccessor)), nme.dropLocalSuffix(field.name).toTermName, Ident(definitions.StringClass), EmptyTree))
    vals :+= ValDef(Modifiers(Set(paramAccessor)), newTermName("id"), Ident(definitions.LongClass), EmptyTree)
    val params = vals map (field => field.copy(mods = Modifiers(Set(parameter, paramAccessor))))
    val body = Apply(Select(Super(This(newTypeName("")), newTypeName("")), nme.CONSTRUCTOR), params.dropRight(1) map (param => Ident(param.name)))
    val ctor = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(params), TypeTree(), Block(List(body), Literal(Constant(()))))
    val tmpl = Template(List(Ident(u), Ident(staticClass("Persisted"))), emptyValDef, vals :+ ctor)
    val cdef = ClassDef(NoMods, newTypeName(c.fresh(u.name + "$Persisted")), Nil, tmpl)

    val temp = ValDef(NoMods, newTermName(c.fresh("precomputed$")), TypeTree(), instance.tree)
    val init = New(Ident(cdef.name), List((params.dropRight(1) map (param => Select(Ident(temp.name), param.name))) :+ id.tree))
    Expr(Block(cdef, temp, init))
  }

}
