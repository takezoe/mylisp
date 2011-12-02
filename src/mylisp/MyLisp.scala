package mylisp
import scala.util.parsing.combinator.RegexParsers

object MyLisp extends App {

  val source = """
    ; recursive function
    (defun factorial (n acc)
      (println n "/" acc)
      (if (<= n 1)
        acc
      (factorial (- n 1) (* acc n))))

    (defun sum (a b)
      (if (eql a 0)
        b
        (sum (- a 1) (+ b a))))

    (println (factorial 10 1))
    ;(println (sum 10000 1))

    ;(println (+ 1 2 3 4))

    ; variable
    ;(setq name "Naoki")
    ;(println name)

    ; list operation
    ;(setq l '(1 2 3 4))
    ;(println l)

    ;(setq l (list 1 2 3 4))
    ;(println (cons 0 l))

    ;(println (null nil))
    ;(println (not '(1 2)))
    ;(println (listp '(1 2 3)))

    ;(println (append '(1 2) '(3 4) '(5 6)))
  """

  println(eval(source))

  def eval(source: String): Any = {
    val parser = new MyLispParser
    val result = parser.parse(removeComment(source))

    println(result)

    val env = new Environment()
    Functions.installGlobalFunctions(env)

    new MyLispVisitor().visit(result.get, env)

  }

  private def removeComment(source: String): String =
    source.replaceAll(";.*", "")

}
