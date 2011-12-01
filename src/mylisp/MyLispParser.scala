package mylisp
import scala.util.parsing.combinator.RegexParsers

class MyLispParser extends RegexParsers {

  def ident :Parser[ASTIdent] = """[A-Za-z_][a-zA-Z0-9]*|\+|-|\*|/|<=|>=|<|>""".r^?{
    case n if n != "defun" && n != "if" => n
  }^^ASTIdent

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => ASTIntVal(value.toInt) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^ASTStrVal

  def symbol: Parser[AST] = ("nil"|"t")^^ASTSymbol

  def defun: Parser[AST] = ("(defun" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ expr <~")")^^{
    case(ident~_~params~_~proc) => {
      ASTDefun(ident.asInstanceOf[ASTIdent], ASTFunc(params.get.asInstanceOf[List[ASTIdent]], proc))
    }
  }

  def `if`: Parser[AST] = ("(if" ~> value ~ value ~ value <~ ")")^^{
    case(cond~expr1~expr2) => ASTIf(cond, expr1, expr2)
  }

  def expr: Parser[AST] = defun|setq|`if`|progn|("(" ~> ident ~ opt(rep(value)) <~ ")" )^^{
    case(ident~params) => {
      ASTExpr(ident.asInstanceOf[ASTIdent], params.get)
    }
  }

  def value: Parser[AST] = expr|symbol|intLiteral|stringLiteral|ident

  def progn: Parser[AST] = "(progn"~>rep(value)<~")"^^{ exprs => ASTProgn(exprs) }

  def setq: Parser[AST] = "(setq"~>ident~value<~")"^^{ case(ident~value) => ASTSetq(ident, value) }

  def program: Parser[AST] = rep(expr)^^{ exprs => ASTProgn(exprs) }

  def parse(str:String) = parseAll(program, str)

}
