package mylisp
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.{Map => MutableMap}

object MyLispParser extends App {

  val source = """
    (defun factorial (n acc)
      (if (<= n 1)
        acc
      (factorial (- n 1) (* acc n))))

    (defun sum (a b)
      (if (eql a 0)
        b
        (sum (- a 1) (+ b a))))

    (println (factorial 10 1))
    (println (sum 10000 1))
    (println (+ 1 2 3 4))

    (setq name "Naoki")
    (println name)
  """

  val parser = new MyLispParser
  println(parser.parse(source))

  val ast = parser.parse(source).get

  val env = new Environment()

  // define global functions
  env.set("println", { params: List[Any] => println(params.mkString) })
  env.set("+", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ + _ } })
  env.set("-", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ - _ } })
  env.set("*", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ * _ } })
  env.set("/", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ / _ } })
  env.set("eql", operator2(_ == _))
  env.set("<=", operator2(_ <= _))
  env.set(">=", operator2(_ >= _))
  env.set("<", operator2(_ < _))
  env.set(">", operator2(_ > _))

  new ExprVisitor().visit(ast, env)

  private def operator2(f: (Int, Int) => Any): List[Any] => Any = {
      (params: List[Any]) => {
        params match {
          case List(a: Int, b: Int) => f(a, b)
          case _ => throw new Exception("Invalid arguments: %s".format(params))
        }
      }
  }
}

class Environment(parent:Option[Environment] = None, val context: Option[Any] = None, last: Boolean = true){

  val variables = MutableMap[String, Any]()

  def get(key:String):Any = {
    if(variables.contains(key)){
      variables(key)
    } else {
      parent match {
        case Some(p) => p.get(key)
        case None => throw new Exception("symbol '%s' not found".format(key))
      }
    }
  }

  def set(key:String, value:Any){
    variables(key) = value
  }
}

class ExprVisitor() {

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

  def processTCO(value: Any, env: Environment): Any = {
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

trait AST
case class IntVal(value: Int) extends AST
case class StrVal(value: String) extends AST
case class BooleanVal(value: Boolean) extends AST
case class Ident(name: String) extends AST
case class Expr(name: Ident, params:List[AST]) extends AST
case class Defun(name: Ident, func: Func) extends AST
case class Func(params:List[Ident], proc:AST) extends AST
case class If(cond:AST, expr1:AST, expr2:AST) extends AST
case class Progn(exprs: List[AST]) extends AST
case class Setq(name: Ident, expr: AST) extends AST

class MyLispParser extends RegexParsers {

  def ident :Parser[Ident] = """[A-Za-z_][a-zA-Z0-9]*|\+|-|\*|/|<=|>=|<|>""".r^?{
    case n if n != "defun" && n != "if" => n
  }^^Ident

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => IntVal(value.toInt) }

  def booleanLiteral : Parser[AST] = ("true"|"false")^^{ value => BooleanVal(value.toBoolean) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^StrVal

  def defun: Parser[AST] = ("(defun" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ expr <~")")^^{
    case(ident~_~params~_~proc) => {
      Defun(ident.asInstanceOf[Ident], Func(params.get.asInstanceOf[List[Ident]], proc))
    }
  }

  def `if`: Parser[AST] = ("(if" ~> value ~ value ~ value <~ ")")^^{
    case(cond~expr1~expr2) => If(cond, expr1, expr2)
  }

  def expr: Parser[AST] = defun|setq|`if`|progn|("(" ~> ident ~ opt(rep(value)) <~ ")" )^^{
    case(ident~params) => {
      Expr(ident.asInstanceOf[Ident], params.get)
    }
  }

  def value: Parser[AST] = expr|intLiteral|stringLiteral|booleanLiteral|ident

  def progn: Parser[AST] = "(progn"~>rep(value)<~")"^^{ exprs => Progn(exprs) }

  def setq: Parser[AST] = "(setq"~>ident~value<~")"^^{ case(ident~value) => Setq(ident, value) }

  def program: Parser[AST] = rep(expr)^^{ exprs => Progn(exprs) }

  def parse(str:String) = parseAll(program, str)

}