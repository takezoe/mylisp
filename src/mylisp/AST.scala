package mylisp

trait AST

case class ASTIntVal(value: Int) extends AST
case class ASTStrVal(value: String) extends AST
case class ASTIdent(name: String) extends AST {
  override def toString(): String = name
}
case class ASTExpr(name: ASTIdent, params:List[AST]) extends AST
case class ASTSymbol(value: String) extends AST
case class ASTList(elements: List[AST]) extends AST {
  override def toString(): String = elements.mkString("ASTList(", ", ", ")")
}
