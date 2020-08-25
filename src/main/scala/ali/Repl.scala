package ali

import cats.implicits._
import ali.Eval.EvalSyntax.resultShow
import com.typesafe.scalalogging.LazyLogging

object Repl extends App with LazyLogging {

  def readEval(nextLine: String)(implicit env: Env): Eval =
    Parser.parse(nextLine) match {
      case Left(l) =>
        Result(Left(s"Parser error: $l"))
      case Right(expr) =>
        logger.trace(s"expr to eval:  $expr")
        try {
          Eval.eval(expr)
        } catch {
          case e: StackOverflowError =>
            Result(Left(s"StackOverflowError at ${e.getStackTrace.last}"))
        }
    }

  @scala.annotation.tailrec
  def repl(implicit env: Env): Unit = {
    val console = System.console
    val nextLine = console.readLine("ali>")

    readEval(nextLine) match {
      case EnvUpdate(newEnv) =>
        logger.trace(s"env update:  $newEnv")
        repl(newEnv)
      case result: Result =>
        logger.trace(s"result:  $result")
        println(result.show)
        repl(env)
      case other =>
        logger.trace(s"other:  $other")
        repl(env)
    }
  }

  repl(Env.root)
}
