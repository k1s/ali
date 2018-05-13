package hexlet

import scala.collection.mutable

class Environment(val parent: Option[Environment]) {

  val map = mutable.Map[Name, Expr]()

  def get(name: Name): Option[Expr] =
    if (map.contains(name))
      map.get(name)
    else
      parent.fold(throw new RuntimeException(s"$name is not defined!"))(_.get(name))

  def add(name: Name, value: Expr): Option[Expr] = map.put(name, value)

}

object Environment {

  def root: Environment = new Environment(None)

}
