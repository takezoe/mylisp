package mylisp
import scala.collection.JavaConversions._

object MyLispVisitor {

  // tail call optimization
  class TCO(val proc: AST, val args: List[Arg], val params: List[AST])

  def visit(ast:AST, env: Environment, last: Boolean = true): Any = {
    ast match {
      case ASTDefun(name, func)    => env.set(name.name, func)
      case ASTDefMacro(name, func) => env.set(name.name, func)
      case ASTIntVal(value)  => value
      case ASTStrVal(value)  => value
      case ASTList(elements) => elements match {
        case (ident: ASTIdent) :: params => {
          env.get(ident.name) match {
            case f: ASTFunc => {
              if(env.context.orNull == f && last){
                // TODO tailcall for macro
                new TCO(f.proc, f.params, params)
              } else {
                val local = new Environment(Some(env), Some(f))
                if(f.macro == true){
                  val fixArgs = f.params.filter(_.rest == false)
                  fixArgs.zip(params).foreach { case(variable, value) =>
                    local.define(variable.name, true)
                    local.set(variable.name, value)
                  }
                  f.params.find(_.rest == true).foreach { variable =>
                    local.define(variable.name, true)
                    local.set(variable.name, params.drop(fixArgs.size))
                  }
                  val result = processTCO(visit(f.proc, local), local)
                  val parser = new MyLispParser
                  val expr = parser.parse(Functions.format(result))
                  visit(expr.get, env)

                } else {
                  val fixArgs = f.params.filter(_.rest == false)
                  fixArgs.zip(params.map(visit(_, env))).foreach { case(variable, value) =>
                    local.define(variable.name, true)
                    local.set(variable.name, value)
                  }
                  f.params.find(_.rest == true).foreach { variable =>
                    local.define(variable.name, true)
                    local.set(variable.name, params.drop(fixArgs.size).map(visit(_, env)))
                  }
                  processTCO(visit(f.proc, local), local)
                }
              }
            }
            case f: EmbedFunction => {
              val local = new Environment(Some(env), Some(f))
              f.invoke(params.map({ e => visit(e, local) }), env)
            }
            case f: EmbedMacro => {
              val local = new Environment(Some(env), Some(f))
              f.invoke(params, env)
            }
            case _ => throw new Exception(s"function '${ident.name}' not found.")
          }
        }
        case _ => throw new Exception(s"Invalid expression: ${ast}")
      }
      case ASTIdent(name)   => env.get(name)
      case ASTSymbol(value) => {
        if(value == "nil"){
          Nil
        } else {
          try {
            DefaultSymbol.withName(value.toUpperCase())
          } catch {
            case ex: NoSuchElementException => UserSymbol(value)
          }
        }
      }
    }
//    ast match {
//      // expression
//      case ASTExpr(ident, params) => {
//        env.get(ident.name) match {
//          case f: ASTFunc => {
//            if(env.context.orNull == f && last){
//              // TODO tailcall for macro
//              new TCO(f.proc, f.params, params)
//            } else {
//              val local = new Environment(Some(env), Some(f))
//              if(f.macro == true){
//                f.params.zip(params).foreach { case(variable, value) =>
//                  local.define(variable.name, true)
//                  local.set(variable.name, value)
//                }
//                val result = processTCO(visit(f.proc, local), local)
//                val parser = new MyLispParser
//                val expr = parser.parseExpression(Functions.format(result))
//                visit(expr.get, env)
//
//              } else {
//                f.params.zip(params.map(visit(_, env))).foreach { case(variable, value) =>
//                  local.define(variable.name, true)
//                  local.set(variable.name, value)
//                }
//                processTCO(visit(f.proc, local), local)
//              }
//            }
//          }
//          case f: ((List[Any]) => Any) => {
//            val local = new Environment(Some(env), Some(f))
//            f(params.map({ e => visit(e, local) }))
//          }
//          case _ => throw new Exception("function '%s' not found.".format(ident.name))
//        }
//      }
//      // if
//      case ASTIf(cond, expr1, expr2) => {
//        if(visit(cond, env) != Nil){
//          visit(expr1, env)
//        } else {
//          visit(expr2, env)
//        }
//      }
//      case ASTIntVal(value) => value
//      case ASTStrVal(value) => value
//      case ASTListVal(elements) => elements.map { visit(_, env) }.toList
//      case ASTIdent(name) => env.get(name)
//      // defun
//      case ASTDefun(name, func) => env.set(name.name, func)
//      // defmacro
//      case ASTDefMacro(name, func) => env.set(name.name, func)
//      // progn
//      case ASTProgn(exprs) => {
//        val last = exprs.last
//        exprs.map({ e => visit(e, env, last == e) }).last
//      }
//      // let
//      case ASTLet(vars, progn) => {
//        val local = new Environment(Some(env), Some(ast)) // TODO context is right?
//        vars.foreach { v =>
//          local.define(v.name.name, true)
//          v.value match {
//            case Some(x) => local.set(v.name.name, visit(x, env))
//            case None =>
//          }
//        }
//        visit(progn, local)
//      }
//      // setf
//      case ASTSetf(name, value) => {
//        env.set(name.name, visit(value, env))
//      }
//      // symbol
//      case ASTSymbol(value) => {
//        if(value == "nil"){
//          Nil
//        } else {
//          try {
//            DefaultSymbol.withName(value.toUpperCase())
//          } catch {
//            case ex: NoSuchElementException => UserSymbol(value)
//          }
//        }
//      }
//    }
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
