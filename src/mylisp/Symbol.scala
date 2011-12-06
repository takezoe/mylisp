package mylisp

trait Symbol {
  def name(): String
  override def toString(): String = name
}

case class UserSymbol(name: String) extends AnyRef with Symbol

object DefaultSymbol extends Enumeration with Symbol{
  val T = Value
  val name = toString()
}