package ali

import Expr.implicits._
import cats.Show
import cats.implicits._

sealed trait Eval {

  def map(f: Env => Env): Eval

  def flatMap(f: Env => Eval): Eval

}

case class EnvUpdate(env: Env) extends Eval {

  override def map(f: Env => Env) = EnvUpdate(f(env))

  override def flatMap(f: Env => Eval) = f(env)

}

case class Result(result: Either[String, Expr]) extends Eval {

  override def map(f: Env => Env) = this

  override def flatMap(f: Env => Eval) = this

}

object Eval {

  type ExprFun = List[Expr] => Expr

  def eval(expr: Expr)(implicit env: Env): Eval =
    expr match {
      case fn: Fun => EnvUpdate(env.addFun(fn))
      case _       => Result(evalExpression(expr))
    }

  def evalExpression(expr: Expr)(implicit env: Env): Either[String, Expr] = {
    println(s"expr $expr")
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            println(s"apply f name $name")
            println(s"apply f args $args")

            env.get(name.id) match {
              case Right(f) =>
                args.traverse(e => evalExpression(e)(env)).flatMap {
                  applyF(f, _)
                }
              case left => left
            }
          case Lambda(lambdaArgs, body) =>
            println(s"apply lambda $fun $args")
            //todo check arity
            val lambdaEnv = lambdaArgs.map(_.id).zip(args).toMap
            evalExpression(body)(Env(env, lambdaEnv))
        }
      case Id(id) =>
        println(s"id $id")
        env.get(id).flatMap(evalExpression)
      case other =>
        println(s"num $other")
        Right(other)
    }
  }

  def applyF(toApply: Expr, args: List[Expr])(
      implicit env: Env): Either[String, Expr] =
    toApply match {
      case Defined(f) =>
        Right(f(args))
      case l: Lambda =>
        evalExpression(Apply(l, args))
      case other =>
        println(" ==== ERRORORO =======")
        println(" ==== ERRORORO =======")
        println(" ==== ERRORORO =======")
        println(other)
        Right(other)
    }

}

object EvalSyntax {

  implicit val resultShow: Show[Result] = _ match {
    case Result(Left(error)) => error
    case Result(Right(expr)) => expr.show
  }

}
