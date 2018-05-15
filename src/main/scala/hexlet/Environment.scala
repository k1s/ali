package hexlet

import hexlet.Expr.ExprFun

import scala.collection.mutable

class Environment(val parent: Option[Environment]) {
//todo immutable?
  private val map = mutable.Map[Name, ExprFun]()

  private def getFromParent(name: Name): ExprFun = parent match {
    case Some(m) => m.get(name)
    case _ => throw new RuntimeException(s"$name is not defined!")
  }

  def get(name: Name): ExprFun = map.getOrElse(name, getFromParent(name))

  def add(name: Name, value: ExprFun): Option[ExprFun] = map.put(name, value)

}

object Environment {

  val root: Environment = {
    val env = new Environment(None)
    env.add(Add, Num.applyNum(Num.add))
    env.add(Sub, Num.applyNum(Num.sub))
    env.add(Mul, Num.applyNum(Num.mul))
    env.add(Div, Num.applyNum(Num.div))
    env
  }

}
