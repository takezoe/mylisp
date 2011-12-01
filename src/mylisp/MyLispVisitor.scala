package mylisp

class MyLispVisitor() {

  // tail call optimization
  class TCO(val proc: AST, val args: List[Ident], val params: List[AST])

  def visit(ast:AST, env: Environment, last: Boolean = true): Any = {
    ast match {
      case Expr(ident, params) => {
        env.get(ident.name) match {
          case f: Func => {
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
      case If(cond, expr1, expr2) => {
        if(visit(cond, env).asInstanceOf[Boolean] == true){
          visit(expr1, env)
        } else {
          visit(expr2, env)
        }
      }
      case IntVal(value) => value
      case StrVal(value) => value
      case BooleanVal(value) => value
      case Ident(name) => env.get(name)
      case Defun(name, func) => env.set(name.name, func)
      case Progn(exprs) => {
        val last = exprs.last
        exprs.map({ e => visit(e, env, last == e) }).last
      }
      case Setq(name, value) => {
        env.set(name.name, visit(value, env))
      }
    }
  }

  private def processTCO(value: Any, env: Environment): Any = {
    var result: Any = value
    while(result.isInstanceOf[TCO]){
      result match {
        case tco: TCO => {
          tco.args.zip(tco.params.map(visit(_, env))).foreach { case(variable, value) =>
            env.set(variable.name, value)
          }
          result = visit(tco.proc, env)
        }
      }
    }
    result
  }

}