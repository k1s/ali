package hexlet


sealed trait Expr

case class Fun(expr: Expr, args: Seq[Expr]) extends Expr

sealed trait Atom extends Expr

case class Num(n: BigInt) extends Atom

class Name(s: String) extends Atom //todo env

case object Add extends Name("+") //todo env






