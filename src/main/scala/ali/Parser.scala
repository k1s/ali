package ali

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  def parse(expr: String): Either[String, Expr] =
    parseAll(hexlet, expr) match {
      case Failure(msg, _) =>
        Left(s"Cannot parse: $msg!")
      case Success(result, t) =>
        Right(result.head)
    }

  def id: Parser[Id] = """[\p{Alnum}[+-/*//]]""".r ^^ { id => Id(id)}

  def number: Parser[Num] = wholeNumber ^^ { x => Num(x.toDouble) }

  def atom: Parser[Atom] = number | id

  def apply: Parser[Apply] =
    "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fun ~ args => Apply(fun, args) }

  def lambda: Parser[Lambda] =
    "(" ~> "\\" ~> rep(id) ~ expr <~ ")" ^^ { case args ~ expr => Lambda(args, expr) }

  def vec: Parser[Vec] = "[" ~> rep(expr) <~ "]" ^^ { exprs => Vec(exprs.toVector) }

  def expr: Parser[Expr] = atom | apply | lambda | vec

  def hexlet: Parser[Seq[Expr]] = rep(expr)

}
