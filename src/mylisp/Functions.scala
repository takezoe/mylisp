package mylisp

object Functions {

  /**
   * Defines global functions.
   */
  def installGlobalFunctions(env: Environment): Unit = {
    env.set("println", { params: List[Any] => println(params.map(format).mkString) })
    env.set("+", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ + _ } })
    env.set("-", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ - _ } })
    env.set("*", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ * _ } })
    env.set("/", { params: List[Any] => params.asInstanceOf[List[Int]].reduceLeft { _ / _ } })
    env.set("eql", operator2( { (a, b) => if(a == b) DefaultSymbol.T else Nil }))
    env.set("<=", operator2({ (a, b) => if(a <= b) DefaultSymbol.T else Nil }))
    env.set(">=", operator2({ (a, b) => if(a >= b) DefaultSymbol.T else Nil }))
    env.set("<", operator2({ (a, b) => if(a < b) DefaultSymbol.T else Nil }))
    env.set(">", operator2({ (a, b) => if(a > b) DefaultSymbol.T else Nil }))
    env.set("list", { params: List[Any] => params })
    env.set("listp", { params: List[Any] =>
      params match {
        case List(e) => e match {
          case Nil => DefaultSymbol.T
          case list: List[Any] => DefaultSymbol.T
          case _ => Nil
        }
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("append", { params: List[Any] =>
      params match {
        case list: List[List[Any]] => list.flatMap { e => e }.toList
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("null", { params: List[Any] =>
      params match {
        case List(e) => e match {
          case Nil => DefaultSymbol.T
          case list: List[Any] if list.isEmpty => DefaultSymbol.T
          case _ => Nil
        }
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("not", env.get("null"))
    env.set("cons", { params: List[Any] =>
      params match {
        case List(first: Any, list: List[Any]) => first :: list
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("car", { params: List[Any] =>
      params match {
        case List(e: List[Any]) => if(e.isEmpty) Nil else e.head
        case _ => throw new IllegalArgumentException(params.toString())
      }
    })
    env.set("cdr", { params: List[Any] =>
      params match {
        case List(e: List[Any]) => if(e.isEmpty) Nil else e.tail
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

  def format(obj: Any): String = {
    obj match {
      case Nil => "nil"
      case list: List[_] => list.map(format).mkString("(", " ", ")")
      case x => x.toString()
    }
  }

}