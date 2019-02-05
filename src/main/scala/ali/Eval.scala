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
        println(s"env update: $fn")
        EnvUpdate(env.addFun(fn))
      case _ =>
        Result(evalExpression(expr))
    }

  def evalExpression(expr: Expr)(implicit env: Env): Either[String, Expr] = {
    println(s"expr $expr")
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            println(s"apply fun name: $name args: $args")
            env.get(name.id) match {
              case Right(f) =>
                args.traverse(e => evalExpression(e)(env)).flatMap {
                  applyF(f, _)
                }
              case left => left
            }
          case Lambda(lambdaArgs, body) =>
            println(s"apply \\ lambda: $fun args: $args")
            closure(body, lambdaArgs.map(_.id), args, Map(), env)
        }
      case Id(id) =>
        println(s"id $id")
        env.get(id).flatMap(evalExpression)
      case n: Num =>
        println(s"num $n")
        Right(n)
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
        println(s"lambda env $closureEnv")
        evalExpression(Apply(body, exs_))(Env(env, closureEnv))
      case (idsHead :: idsTail, exsHead :: exsTail) =>
        closure(body, idsTail, exsTail, closureEnv + (idsHead -> exsHead), env)
      case _ =>
        Left(s"Lambda args size error: $ids != $exprs")
    }

  def applyF(toApply: Expr, args: List[Expr])(implicit env: Env): Either[String, Expr] =
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
