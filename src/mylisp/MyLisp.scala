package mylisp
import scala.util.parsing.combinator.RegexParsers

object MyLisp extends App {

  val source = """
    ; recursive function
    (defun factorial (n acc)
      (if (<= n 1)
        acc
      (factorial (- n 1) (* acc n))))

    (defun sum (a b)
      (if (eql a 0)
        b
        (sum (- a 1) (+ b a))))

    (factorial 10 1)

    (defmacro nil! (var)
      (list 'setf var nil))

    (setf x "Hello World!")
    (nil! x)
    (println x)
  """

  println(Functions.format(eval(source)))

  def eval(source: String): Any = {
    val parser = new MyLispParser
    val result = parser.parse("(" + removeComment(source) + ")")

    // TODO for debug
    println(result)

//    val env = new Environment()
//    Functions.installGlobalFunctions(env)
//
//    new MyLispVisitor().visit(result.get, env)
  }

  private def removeComment(source: String): String =
    source.replaceAll(";.*", "")
}
