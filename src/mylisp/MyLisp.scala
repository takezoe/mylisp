package mylisp
import scala.util.parsing.combinator.RegexParsers

object MyLisp extends App {

  val source = """
    (defun factorial (n acc)
      (if (<= n 1)
        acc
      (factorial (- n 1) (* acc n))))

    (defun sum (a b)
      (if (eql a 0)
        b
        (sum (- a 1) (+ b a))))

    (println (factorial 10 1))
    (println (sum 10000 1))
    (println (+ 1 2 3 4))

    (setq name "Naoki")
    (println name)

    (setq l '(1 2 3 4))
    (println l)

    (setq l (list 1 2 3 4))
    (println (cons 0 l))
    (println (cdr (cons 0 l)))
    (println (car (cons 0 l)))
  """

  val parser = new MyLispParser
  val result = parser.parse(source)

  println(result)

  val env = new Environment()
  installGlobalFunctions(env)

  new MyLispVisitor().visit(result.get, env)

  /**
   * Defines global functions.
   */
  private def installGlobalFunctions(env: Environment): Unit = {
    env.set("println", { params: List[Any] => println(params.mkString) })
    env.set("+", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ + _ } })
    env.set("-", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ - _ } })
    env.set("*", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ * _ } })
    env.set("/", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ / _ } })
    env.set("eql", operator2( { (a, b) => if(a == b) Symbol.T else Symbol.NIL }))
    env.set("<=", operator2({ (a, b) => if(a <= b) Symbol.T else Symbol.NIL }))
    env.set(">=", operator2({ (a, b) => if(a >= b) Symbol.T else Symbol.NIL }))
    env.set("<", operator2({ (a, b) => if(a < b) Symbol.T else Symbol.NIL }))
    env.set(">", operator2({ (a, b) => if(a > b) Symbol.T else Symbol.NIL }))
    env.set("list", { params: List[Any] => params })
    env.set("cons", { params: List[Any] =>
      params match {
        case List(first: Any, list: List[Any]) => first :: list
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("car", { params: List[Any] =>
      params match {
        case List(e: List[Any]) => e.head
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("cdr", { params: List[Any] =>
      params match {
        case List(e: List[Any]) => e.tail
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })

    def operator2(f: (Int, Int) => Any): List[Any] => Any = {
      (params: List[Any]) => {
        params match {
          case List(a: Int, b: Int) => f(a, b)
          case _ => throw new Exception("Invalid arguments: %s".format(params))
        }
      }
    }
  }
}
