package ali

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  def parse(expr: String): Either[String, Expr] =
    parseAll(hexlet, expr) match {
      case Failure(msg, _) =>
        Left(s"Cannot parse: $msg!")
      case Success(result, _) =>
        Right(result.head)
    }

   def id: Parser[Id] = """[a-zA-Z-_+*=<>\/][a-zA-Z0-9]*""".r ^^ { id => Id(id)}

  def number: Parser[Num] = """(0|[1-9]\d*)""".r ^^ { x => Num(x.toDouble) }

  def atom: Parser[Atom] = number | id

  def apply: Parser[Apply] =
    "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fun ~ args => Apply(fun, args) }

  def vec: Parser[Vec] = "[" ~> rep(expr) <~ "]" ^^ { exprs => Vec(exprs.toVector) }

  def lambda: Parser[Lambda] =
    "(" ~> "\\" ~> rep(id) ~ expr <~ ")" ^^ { case args ~ expr => Lambda(args, expr) }

  def `def`: Parser[Def] =
    "(" ~> "def" ~> id ~ expr <~ ")" ^^ { case name ~ expr => Def(name, expr) }

  def `if`: Parser[If] =
    "(" ~> "if" ~> expr ~ expr ~ expr <~ ")" ^^ { case e1 ~ e2 ~ e3 => If(e1, e2, e3) }

  def expr: Parser[Expr] = `def` | atom | `if` | lambda | vec | apply

  def hexlet: Parser[Seq[Expr]] = rep(expr)

}
