package mylisp

trait AST

case class ASTIntVal(value: Int) extends AST
case class ASTStrVal(value: String) extends AST
case class ASTIdent(name: String) extends AST
case class ASTExpr(name: ASTIdent, params:List[AST]) extends AST
case class ASTDefun(name: ASTIdent, func: ASTFunc) extends AST
case class ASTFunc(params:List[ASTIdent], proc:AST) extends AST
case class ASTIf(cond:AST, expr1:AST, expr2:AST) extends AST
case class ASTProgn(exprs: List[AST]) extends AST
case class ASTSetq(name: ASTIdent, expr: AST) extends AST
case class ASTSymbol(value: String) extends AST
case class ASTListVal(elements: List[AST]) extends AST
