package mylisp

trait AST

case class ASTIntVal(value: Int) extends AST
case class ASTStrVal(value: String) extends AST
case class ASTIdent(name: String) extends AST {
  override def toString(): String = name
}
case class ASTExpr(name: ASTIdent, params:List[AST]) extends AST
case class ASTDefun(name: ASTIdent, func: ASTFunc) extends AST
case class ASTDefMacro(name: ASTIdent, func: ASTFunc) extends AST
case class ASTFunc(params:List[ASTIdent], proc:AST, macro: Boolean = false) extends AST
case class ASTIf(cond:AST, expr1:AST, expr2:AST) extends AST
case class ASTProgn(exprs: List[AST]) extends AST
case class ASTSetf(name: ASTIdent, expr: AST) extends AST
case class ASTSymbol(value: String) extends AST
case class ASTListVal(elements: List[AST]) extends AST
case class ASTVar(name: ASTIdent, value: Option[AST]) extends AST
case class ASTLet(vars: List[ASTVar], proc: ASTProgn) extends AST
