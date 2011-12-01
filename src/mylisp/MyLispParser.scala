package mylisp
import scala.util.parsing.combinator.RegexParsers

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
