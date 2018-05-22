package ali

import ali.Expr.ExprFun

sealed trait Expr

case class Apply(expr: Expr, args: Seq[Expr]) extends Expr

case class Lambda(args: Seq[Id], body: Expr) extends Expr

case class Vec(vs: Vector[Expr]) extends Expr

object Vec { def apply(xs: Expr*): Vec = new Vec(xs.toVector) }

trait Atom extends Expr

case class Id(id: String) extends Atom

case class Num(n: Double) extends Atom

object Expr {

  object implicits {
//todo here must be not such ugly way
    implicit class ExprFunOps(exprFun: ExprFun) {
      def const = exprFun.apply(Seq())
    }

  }

  type ExprFun = Seq[Expr] => Expr
//todo here must be not such ugly way
  def lift(a: Expr): ExprFun = (_: Seq[Expr]) => a

}

object Fold {

  type Fold = (Expr, Expr) => Expr

  def fold(f: Fold): ExprFun = (exprs: Seq[Expr]) =>
    exprs.tail.foldLeft(exprs.head)(f)

  val add: ExprFun = fold {
    case (n1: Num, n2: Num) => Num(n1.n + n2.n)
    case (v1: Vec, v2: Vec) => Vec(v1.vs ++ v2.vs)
  }

  val sub: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n - n2.n) }
  val mul: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n * n2.n) }
  val div: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n / n2.n) }

}






