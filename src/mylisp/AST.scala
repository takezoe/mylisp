package mylisp

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
