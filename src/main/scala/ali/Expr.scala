package ali

import ali.Expr.ExprFun


sealed trait Expr

case class Apply(expr: Expr, args: Seq[Expr]) extends Expr

trait Atom extends Expr

case class Id(id: String) extends Atom

case class Lambda(args: Seq[Id], body: Expr) extends Expr

case class Num(n: Double) extends Atom

object Expr {

  type ExprFun = Seq[Expr] => Expr
//todo here must be not such ugly way
  def lift(a: Expr): ExprFun = (_: Seq[Expr]) => a

  implicit class ExprFunOps(exprFun: ExprFun) {
    def const = exprFun.apply(Seq())
  }

}


object Num {

  type NumFun = (Num, Num) => Num

  val zero = Num(0)

  val add: NumFun = (n1, n2) => Num(n1.n + n2.n)
  val sub: NumFun = (n1, n2) => Num(n1.n - n2.n)
  val mul: NumFun = (n1, n2) => Num(n1.n * n2.n)
  val div: NumFun = (n1, n2) => Num(n1.n / n2.n)

  def applyNum(f: NumFun): ExprFun = {
      case nums: Seq[Num] =>
        nums.tail.foldLeft(nums.head)(f)
    }

}






