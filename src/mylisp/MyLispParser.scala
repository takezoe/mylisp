package mylisp
import scala.util.parsing.combinator.RegexParsers

class MyLispParser extends RegexParsers {

  def ident :Parser[ASTIdent] = """[A-Za-z_][a-zA-Z0-9]*|\+|-|\*|/|<=|>=|<|>""".r^?{
    case n if n != "defun" && n != "if" => n
  }^^ASTIdent

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => ASTIntVal(value.toInt) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^ASTStrVal

  def symbol: Parser[AST] = ("nil"|"t")^^ASTSymbol

  def defun: Parser[AST] = ("(defun" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ rep(expr) <~")")^^{
    case(ident~_~params~_~proc) => {
      ASTDefun(ident.asInstanceOf[ASTIdent], ASTFunc(params.get.asInstanceOf[List[ASTIdent]], ASTProgn(proc)))
    }
  }

  def `if`: Parser[AST] = ("(if" ~> value ~ value ~ value <~ ")")^^{
    case(cond~expr1~expr2) => ASTIf(cond, expr1, expr2)
  }

  def list: Parser[AST] = "'("~>rep(value)<~")"^^ASTListVal

  def expr: Parser[AST] = defun|setf|`if`|let|progn|("(" ~> ident ~ opt(rep(value)) <~ ")" )^^{
    case(ident~params) => {
      ASTExpr(ident.asInstanceOf[ASTIdent], params.get)
    }
  }

  def value: Parser[AST] = expr|symbol|intLiteral|stringLiteral|ident|list

  def progn: Parser[AST] = "(progn"~>rep(value)<~")"^^{ exprs => ASTProgn(exprs) }

  def setf: Parser[AST] = "(setf"~>ident~value<~")"^^{ case(ident~value) => ASTSetf(ident, value) }

  def `var`: Parser[AST] = (ident|("("~>ident~value<~")"))^^{
    case (ident~value) =>ASTVar(ident.asInstanceOf[ASTIdent], Some(value.asInstanceOf[AST]))
    case (ident) => ASTVar(ident.asInstanceOf[ASTIdent], None)
  }

  def let: Parser[AST] = "(let"~"("~>rep(`var`)~")"~rep(value)<~")"^^{
    case (vars~_~exprs) => ASTLet(vars.asInstanceOf[List[ASTVar]], ASTProgn(exprs))
  }

  def program: Parser[AST] = rep(expr)^^{ exprs => ASTProgn(exprs) }

  def parse(str:String) = parseAll(program, str)

}
