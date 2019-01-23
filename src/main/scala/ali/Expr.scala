package ali

import Eval.ExprFun
import cats.Show

sealed trait Expr

case class Apply(expr: Expr, args: List[Expr]) extends Expr

case class Lambda(args: List[Id], body: Expr) extends Expr

case class Fun(name: Id, body: Expr) extends Expr

case class Defined(exprFun: ExprFun) extends Expr

case class Vec(vs: Vector[Expr]) extends Expr

object Vec { def apply(xs: Expr*): Vec = new Vec(xs.toVector) }

trait Atom extends Expr

case class Id(id: String) extends Atom

case class Num(n: Double) extends Atom

object Expr {

  object implicits {

    implicit val showExpr: Show[Expr]  = _ match {
      case Num(n) => n.toString
      case Id(id) => id.toString
      case other  => other.toString
    }

  }

}








