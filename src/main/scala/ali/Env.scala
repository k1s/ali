package ali

import ali.Expr.ExprFun


class Env(val parent: Option[Env], val map: Map[String, ExprFun]) {

  //todo errors that not fail repl
  private def getFromParent(name: String): ExprFun = parent match {
    case Some(m) => m.get(name)
    case _ => throw new RuntimeException(s"$name is not defined!")
  }

  def get(name: String): ExprFun = map.getOrElse(name, getFromParent(name))

}

object Env {

  def apply(parent: Env, map: Map[String, ExprFun]): Env =
    new Env(Some(parent), map)

  def apply(map: Map[String, ExprFun]): Env =
    new Env(None, map)

  val root: Env =
    Env(
      Map(
        "+" -> Fold.add,
        "-" -> Fold.sub,
        "*" -> Fold.mul,
        "/" -> Fold.div))

}
