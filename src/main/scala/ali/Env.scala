package ali

import ali.Eval.ExprFun

class Env(val parent: Option[Env], val current: Map[String, Expr]) {

  private def getFromParent(name: String): Either[String, Expr] = parent match {
    case Some(m) => m.get(name)
    case _       => Left(s"$name is not defined!")
  }

  def get(name: String): Either[String, Expr] = current get name match {
    case Some(v) => Right(v)
    case None    => getFromParent(name)
  }

  def addFun(fun: Fun): Env = {
    Env(this, Map(fun.name.id -> fun.body))
  }

}

object Env {

  def apply(parent: Env, map: Map[String, Expr]): Env =
    new Env(Some(parent), map)

  def apply(map: Map[String, Expr]): Env =
    new Env(None, map)

  def fold(f: (Expr, Expr) => Expr): ExprFun =
    (exprs: Seq[Expr]) => exprs.tail.foldLeft(exprs.head)(f)

  val add: ExprFun = fold {
    case (n1: Num, n2: Num) => Num(n1.n + n2.n)
    case (v1: Vec, v2: Vec) => Vec(v1.vs ++ v2.vs)
  }

  val sub: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n - n2.n) }
  val mul: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n * n2.n) }
  val div: ExprFun = fold { case (n1: Num, n2: Num) => Num(n1.n / n2.n) }

  val root: Env =
    Env(
      Map("+" -> Defined(add),
          "-" -> Defined(sub),
          "*" -> Defined(mul),
          "/" -> Defined(div))
    )

}
