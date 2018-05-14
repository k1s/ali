package hexlet

import hexlet.Expr.ExprFun

sealed trait Expr

case class Fun(expr: Expr, args: Seq[Expr]) extends Expr

sealed trait Atom extends Expr

case class Num(n: BigInt) extends Atom

class Name(s: String) extends Atom

case object Add extends Name("+")

object Expr {

  type ExprFun = Seq[Expr] => Expr

}

object Num {

  type NumFun = (Num, Num) => Num

  val zero = Num(0)

  val add: NumFun = (n1, n2) => Num(n1.n + n2.n)

  def applyNum(f: NumFun): ExprFun = { case args: Seq[Num] =>
    args.tail.foldLeft(args.head)(f) }

}






