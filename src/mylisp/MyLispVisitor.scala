package mylisp
import scala.collection.JavaConversions._

class MyLispVisitor() {

  // tail call optimization
  class TCO(val proc: AST, val args: List[ASTIdent], val params: List[AST])

  def visit(ast:AST, env: Environment, last: Boolean = true): Any = {
    ast match {
      case ASTExpr(ident, params) => {
        env.get(ident.name) match {
          case f: ASTFunc => {
            if(env.context.orNull == f && last){
              new TCO(f.proc, f.params, params)
            } else {
              val local = new Environment(Some(env), Some(f))
              f.params.zip(params.map(visit(_, env))).foreach { case(variable, value) =>
                local.set(variable.name, value)
              }
              processTCO(visit(f.proc, local), local)
            }
          }
          case f: ((List[Any]) => Any) => {
            val local = new Environment(Some(env), Some(f))
            f(params.map({ e => visit(e, local) }))
          }
          case _ => throw new Exception("function '%s' not found.".format(ident.name))
        }
      }
      case ASTIf(cond, expr1, expr2) => {
        if(visit(cond, env) != Symbol.NIL){
          visit(expr1, env)
        } else {
          visit(expr2, env)
        }
      }
      case ASTIntVal(value) => value
      case ASTStrVal(value) => value
      case ASTListVal(elements) => elements.map { visit(_, env) }.toList
      case ASTIdent(name) => env.get(name)
      case ASTDefun(name, func) => env.set(name.name, func)
      case ASTProgn(exprs) => {
        val last = exprs.last
        exprs.map({ e => visit(e, env, last == e) }).last
      }
      case ASTSetq(name, value) => {
        env.set(name.name, visit(value, env))
      }
      case ASTSymbol(value) => Symbol.withName(value.toUpperCase())
    }
  }

  private def processTCO(value: Any, env: Environment): Any = {
    var result: Any = value
    while(result.isInstanceOf[TCO]){
      val tco = result.asInstanceOf[TCO]
      tco.args.zip(tco.params.map(visit(_, env))).foreach { case(variable, value) =>
        env.set(variable.name, value)
      }
      result = visit(tco.proc, env)
    }
    result
  }

}
