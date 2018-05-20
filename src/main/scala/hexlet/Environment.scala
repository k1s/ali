package hexlet

import hexlet.Expr.ExprFun

import scala.collection.mutable

class Environment(val parent: Option[Environment]) {
//todo immutable?
  private val map = mutable.Map[String, ExprFun]()
//todo errors that not fail repl
  private def getFromParent(name: String): ExprFun = parent match {
    case Some(m) => m.get(name)
    case _ => throw new RuntimeException(s"$name is not defined!")
  }

  def get(name: String): ExprFun = map.getOrElse(name, getFromParent(name))

  def add(name: String, value: ExprFun): Option[ExprFun] = map.put(name, value)

}

object Environment {

  val root: Environment = {
    val env = new Environment(None)
    env.add("+", Num.applyNum(Num.add))
    env.add("-", Num.applyNum(Num.sub))
    env.add("*", Num.applyNum(Num.mul))
    env.add("/", Num.applyNum(Num.div))
    env
  }

}
