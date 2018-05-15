package hexlet

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  def parse(expr: String): Expr =
    parseAll(hexlet, expr) match {
      case Failure(msg, _) =>
        throw new RuntimeException(s"Cannot into $msg!")
      case Success(result, _) =>
        result.head //todo tail
    }

  def name: Parser[Name] = "[+]".r ^^ { _ => Add}

  def number: Parser[Num] = wholeNumber ^^ { x => Num(x.toDouble) }

  def atom: Parser[Atom] = number | name

  def fun: Parser[Fun] = "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fun ~ args => Fun(fun, args) }

  def expr: Parser[Expr] = atom | fun

  def hexlet: Parser[Seq[Expr]] = rep(expr)

}
