package ali

import ali.Expr.ExprFun


class Env(val parent: Option[Env], val current: Map[String, ExprFun]) {

  private def getFromParent(name: String): Either[String, ExprFun] = parent match {
    case Some(m) => m.get(name)
    case _       => Left(s"$name is not defined!")
  }

  def get(name: String): Either[String, ExprFun] = current get name match {
    case Some(v) => Right(v)
    case None    => getFromParent(name)
  }

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
