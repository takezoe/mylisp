package mylisp
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.{Map => MutableMap}

object MyLispParser extends App {

  val source = """
    (defun sayHello (name) (println "Hello " name "!"))
    (sayHello "World")
  """

  val parser = new MyLispParser
  println(parser.parse(source))

  val ast = parser.parse(source).get

  val env = new Environment()

  // define global functions
  env.set("println", { params: List[Any] =>
    println(params.mkString)
  })
  env.set("+", { params: List[Any] =>
    params match {
      case(List(a: Int, b: Int)) => a + b
      case _ => throw new Exception("parameter is invalid.")
    }
  })
  env.set("-", { params: List[Any] =>
    params match {
      case(List(a: Int, b: Int)) => a - b
      case _ => throw new Exception("parameter is invalid.")
    }
  })
  env.set("*", { params: List[Any] =>
    params match {
      case(List(a: Int, b: Int)) => a * b
      case _ => throw new Exception("parameter is invalid.")
    }
  })
  env.set("/", { params: List[Any] =>
    params match {
      case(List(a: Int, b: Int)) => a / b
      case _ => throw new Exception("parameter is invalid.")
    }
  })

  new ExprVisitor().visit(ast, env)
}

class Environment(parent:Option[Environment] = None){

  val variables = MutableMap[String, Any]()

  def get(key:String):Any = {
    if(variables.contains(key)){
      variables(key)
    } else {
      parent match {
        case Some(p) => p.get(key)
        case None => throw new Exception("symbol'%s' not found".format(key))
      }
    }
  }

  def set(key:String, value:Any){
    variables(key) = value
  }
}

class ExprVisitor() {
  def visit(ast:AST, env: Environment):Any = {
    ast match {
      case Expr(name, params) => {
        env.get(name.name) match {
          case f: Func => {
            val local = new Environment(Some(env))
            f.params.zip(params.map(visit(_, env))).foreach { case(variable, value) =>
              local.set(variable.name, value)
            }
            visit(f.proc, local)
          }
          case f: ((List[Any]) => Any) => f(params.map(visit(_, env)))
          case _ => throw new Exception("function '%s' not found.".format(name.name))
        }
      }
      case IntVal(value) => value
      case StrVal(value) => value
      case Ident(name) => env.get(name)
      case Defun(name, func) => env.set(name.name, func)
      case Program(exprs) => exprs.foreach(visit(_, env))
    }
  }
}

trait AST
case class IntVal(value: Int) extends AST
case class StrVal(value: String) extends AST
case class Ident(name: String) extends AST
case class Expr(name: Ident, params:List[AST]) extends AST
case class Defun(name: Ident, func: Func) extends AST
case class Func(params:List[Ident], proc:AST) extends AST
case class Program(exprs: List[AST]) extends AST

class MyLispParser extends RegexParsers {

  def ident :Parser[Ident] = """[A-Za-z_+\-*/][a-zA-Z0-9]*""".r^?{
    case n if n != "defun" => n
  }^^Ident

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => IntVal(value.toInt) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^StrVal

  def value: Parser[AST] = expr|intLiteral|stringLiteral|ident

  def defun: Parser[AST] = ("(defun" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ expr <~")")^^{
    case(ident~_~params~_~proc) => {
      Defun(ident.asInstanceOf[Ident], Func(params.get.asInstanceOf[List[Ident]], proc))
    }
  }

  def expr: Parser[AST] = defun|("(" ~> ident ~ opt(rep(value)) <~ ")" )^^{
    case(ident~params) => {
      Expr(ident.asInstanceOf[Ident], params.get)
    }
  }

  def program: Parser[AST] = rep(expr)^^{ exprs => Program(exprs) }

  def parse(str:String) = parseAll(program, str)

}