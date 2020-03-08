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
      case fn: Fun =>
        EnvUpdate(env.addFun(fn))
      case _ =>
        Result(evalExpression(expr))
    }

  def evalExpression(expr: Expr)(implicit env: Env): Either[String, Expr] = {
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            env.get(name.id) match {
              case Right(f) =>
                args.traverse(e => evalExpression(e)(env)).flatMap {
                  applyF(f, _)
                }
              case left => left
            }
          case Lambda(lambdaArgs, body) =>
            closure(body, lambdaArgs.map(_.id), args, Map(), env)
        }
      case Id(id) =>
        env.get(id).flatMap(evalExpression)
      case f: Foldable =>
        Right(f)
      case other =>
        Left(s"Evaluation of $other is impossibru!!!")
    }
  }

  @scala.annotation.tailrec
  def closure(body: Expr,
              ids: List[String],
              exprs: List[Expr],
              closureEnv: Map[String, Expr],
              env: Env): Either[String, Expr] =
    (ids, exprs) match {
      case (Nil, Nil) =>
        evalExpression(body)(Env(env, closureEnv))
      case (Nil, exs_) =>
        evalExpression(Apply(body, exs_))(Env(env, closureEnv))
      case (idsHead :: idsTail, exsHead :: exsTail) =>
        closure(body, idsTail, exsTail, closureEnv + (idsHead -> exsHead), env)
      case _ =>
        Left(s"Lambda args size error: $ids != $exprs")
    }

  def applyF(toApply: Expr, args: List[Expr])(implicit env: Env): Either[String, Expr] =
    toApply match {
      case Predefined(f) =>
        Right(f(args))
      case l: Lambda =>
        evalExpression(Apply(l, args))
      case other =>
        Right(other)
    }

}

object EvalSyntax {

  implicit val resultShow: Show[Result] = {
    case Result(Left(error)) => error
    case Result(Right(expr)) => expr.show
  }

}
