package mylisp
import scala.util.parsing.combinator.RegexParsers

class MyLispParser extends RegexParsers {

  def ident :Parser[ASTIdent] = """&?[A-Za-z_][a-zA-Z0-9!]*|\+|-|\*|/|<=|>=|<|>|!""".r^^ASTIdent

  def intLiteral : Parser[AST] = """[1-9][0-9]*|0""".r^^{ value => ASTIntVal(value.toInt) }

  def stringLiteral : Parser[AST] = "\""~>"""[a-zA-Z0-9:*/+\- !]*""".r<~"\""^^ASTStrVal

  def symbol: Parser[AST] = "'"~>ident^^{ value => ASTSymbol(value.name) }|("nil"|"t")^^ASTSymbol

  def list: Parser[AST] = "("~>rep(value)<~")"^^ASTList

  def value: Parser[AST] = defun|list|symbol|intLiteral|stringLiteral|ident

  def defun: Parser[AST] = ("(defun" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ list <~")")^^{
    case(ident~_~params~_~list) => {
      val args = params.getOrElse(Nil)
      val fixArgs = args.takeWhile(_.name != "&rest").map                    { x => Arg(x.name) }
      val varArgs = args.dropWhile(_.name != "&rest").drop(1).headOption.map { x => Arg(x.name, true)}
      ASTDefun(ident.asInstanceOf[ASTIdent], ASTFunc(fixArgs ++ varArgs.map(List(_)).getOrElse(Nil), list))
    }
  }

//  def defmacro: Parser[AST] = ("(defmacro" ~> ident ~"("~ opt(rep(ident)) ~ ")" ~ list <~")")^^{
//    case(ident~_~params~_~list) => {
//      ASTDefMacro(ident.asInstanceOf[ASTIdent], ASTFunc(params.get.asInstanceOf[List[ASTIdent]], list, true))
//    }
//  }
  
  def parse(str:String) = parseAll(list, str)
  
}
