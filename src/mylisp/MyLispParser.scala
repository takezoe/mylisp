package mylisp
import scala.util.parsing.combinator.RegexParsers

class MyLispParser extends RegexParsers {

  def ident :Parser[ASTIdent] = """[A-Za-z_][a-zA-Z0-9!]*|\+|-|\*|/|<=|>=|<|>|!""".r^^ASTIdent

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => ASTIntVal(value.toInt) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^ASTStrVal

  def symbol: Parser[AST] = "'"~>ident^^{ value => ASTSymbol(value.name) }|("nil"|"t")^^ASTSymbol

  def list: Parser[AST] = "("~>rep(value)<~")"^^ASTList

  def value: Parser[AST] = list|symbol|intLiteral|stringLiteral|ident

  def parse(str:String) = parseAll(list, str)
  
}
