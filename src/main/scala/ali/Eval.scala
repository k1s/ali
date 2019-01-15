package ali

import Expr.implicits._
import cats.implicits._


object Eval {

  def eval(expr: Expr)(implicit env: Env): Either[String, Expr] = {
    println(s"expr $expr")
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            println(s"apply name $fun $args")

            args.toList.traverse(e => eval(e)(env)).flatMap { applyF(name, _) }
          case Lambda(lambdaArgs, body) =>
            println(s"apply lambda $fun $args")
            //todo check arity
            val lambdaEnv = lambdaArgs.map(_.id).zip(args.map(Expr.lift)).toMap
            eval(body)(Env(env, lambdaEnv))
        }
      case Id(id) =>
        println(s"id $id")
        env.get(id).map(_.const).flatMap(eval)
      case other =>
        println(s"num $other")
        Right(other)
    }
  }

  def applyF(name: Id, args: Seq[Expr])(implicit env: Env): Either[String, Expr] = {
    println(s"apply f name $name")
    println(s"apply f args $args")

    if (args.size == 1)
      eval(args.head)
    else
      env.get(name.id).map(f => f(args))
  }

}
