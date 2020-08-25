package ali

import Eval.ExprFun
import cats.Show

sealed trait Expr

sealed trait Foldable

case class Predefined(exprFun: ExprFun) extends Expr

case class Apply(expr: Expr, args: List[Expr]) extends Expr

case class Lambda(args:List[Id], body: Expr) extends Expr

/*
  It's just relation between name(id) and anything, lambda or scalar
 */
case class Def(name: Id, expr: Expr) extends Expr

case class If(test: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

case class Vec(vs: Vector[Expr]) extends Expr with Foldable

object Vec { def apply(xs: Expr*): Vec = new Vec(xs.toVector) }

trait Atom extends Expr

case class Id(id: String) extends Atom

case class Num(n: Double) extends Atom with Foldable

case class Bool(b: Boolean) extends Atom

object Expr {

  object implicits {

    implicit val show: Show[Expr]  = _.toString

    implicit val ordering: Ordering[Expr] = (e1, e2) =>
      (e1, e2) match {
        case (Num(n1), Num(n2)) => n1.compareTo(n2)
      }

  }

}








