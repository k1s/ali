package hexlet

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  def parse(expr: String): Expr =
    parseAll(hexlet, expr) match {
      case Failure(msg, _) =>
        throw new RuntimeException(s"Cannot parse: $msg!")
      case Success(result, _) =>
        result.head //todo tail
    }

  def id: Parser[Id] = """[\p{Alnum}[+-/*//]]""".r ^^ { id => Id(id)}

  def number: Parser[Num] = wholeNumber ^^ { x => Num(x.toDouble) }

  def atom: Parser[Atom] = number | id

  def apply: Parser[Apply] =
    "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fun ~ args => Apply(fun, args) }

  def fun: Parser[Fun] =
    "(" ~> "\\" ~> rep(id) ~ expr <~ ")" ^^ { case args ~ expr => Fun(args, expr) }

  def expr: Parser[Expr] = atom | apply | fun

  def hexlet: Parser[Seq[Expr]] = rep(expr)

}
