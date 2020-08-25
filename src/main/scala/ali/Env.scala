package ali

import Eval.ExprFun
import Expr.implicits.ordering

class Env(val parent: Option[Env],
          val current: Map[String, Expr]) {

  private def getFromParent(name: String): Either[String, Expr] = parent match {
    case Some(m) => m.get(name)
    case _       => Left(s"$name is not defined in env")
  }

  def get(name: String): Either[String, Expr] = current get name match {
    case Some(v) => Right(v)
    case None    => getFromParent(name)
  }

  def addDef(`def`: Def): Env = {
    Env(this, Map(`def`.name.id -> `def`.expr))
  }

  override def toString =
    s"""
      |ENV: ==>
      |parent: $parent
      |current: $current
      """.stripMargin

}

object Env {

  def apply(parent: Env, map: Map[String, Expr]): Env =
    new Env(Some(parent), map)

  def apply(map: Map[String, Expr]): Env =
    new Env(None, map)

  def fold(f: (Expr, Expr) => Expr): ExprFun = exprs =>
    exprs.tail.foldLeft(exprs.head)(f)

  val add: ExprFun = fold {
    case (n1: Num, n2: Num) => Num(n1.n + n2.n)
    case (v1: Vec, v2: Vec) => Vec(v1.vs ++ v2.vs)
  }

  val sub: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n - n2.n) }
  val mul: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n * n2.n) }
  val div: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n / n2.n) }

  def predicateFold(predicate: (Expr, Expr) => Boolean): ExprFun = exprs => {
    val (bool, _) = exprs.tail.foldLeft((true, exprs.head)) { case ((acc, prev), next) =>
      (acc && predicate(prev, next), next)
    }

    Bool(bool)
  }

  val equals: ExprFun = predicateFold { (e1, e2) => e1 == e2 }
  val less: ExprFun = predicateFold { (e1, e2) => Ordering[Expr].lt(e1, e2) }
  val greater: ExprFun = predicateFold { (e1, e2) => Ordering[Expr].gt(e1, e2) }

  val root: Env =
    Env(
      Map("+" -> Predefined(add),
          "-" -> Predefined(sub),
          "*" -> Predefined(mul),
          "/" -> Predefined(div),
          "=" -> Predefined(equals),
          "<" -> Predefined(less),
          ">" -> Predefined(greater),
      )
    )

}
