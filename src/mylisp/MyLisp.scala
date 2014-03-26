package mylisp
import scala.util.parsing.combinator.RegexParsers

object MyLisp extends App {

  val source = """
    ; recursive function
    ;(defun hello1 (x) (println x))
    ;(defun hello2 (x &rest z) (progn (println x) (println z)))
    ;(hello1 "Hello MyLisp!")
    ;(hello2 "Hello MyLisp!" 1 2 3)
    
    (defmacro nil! (var)
      (list 'setf var nil))

    (setf x "Hello World!")
    (nil! x)
    (println x)
  """

  println(Functions.format(eval(source)))

  def eval(source: String): Any = {
    val parser = new MyLispParser
    val result = parser.parse("(progn " + removeComment(source) + ")")

    // TODO for debug
    println(result)

    val env = new Environment()
    Functions.installGlobalFunctions(env)

    MyLispVisitor.visit(result.get, env)
  }

  private def removeComment(source: String): String =
    source.replaceAll(";.*", "")
}
