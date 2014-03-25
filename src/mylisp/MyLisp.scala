package mylisp
import scala.util.parsing.combinator.RegexParsers

object MyLisp extends App {

  val source = """
    ; recursive function
    (println "Hello MyLisp!")
  """

  println(Functions.format(eval(source)))

  def eval(source: String): Any = {
    val parser = new MyLispParser
    val result = parser.parse("(progn " + removeComment(source) + ")")

    // TODO for debug
    println(result)

    val env = new Environment()
    Functions.installGlobalFunctions(env)

    new MyLispVisitor().visit(result.get, env)
  }

  private def removeComment(source: String): String =
    source.replaceAll(";.*", "")
}
