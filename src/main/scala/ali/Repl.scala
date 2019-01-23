package ali

import cats.implicits._
import EvalSyntax.resultShow

object Repl extends App {

  lazy val error = "Unexpected error"

  def readEval(nextLine: String)(implicit env: Env): Eval =
    Parser.parse(nextLine) match {
        case Left(l)      => Result(Left(s"$error: $l"))
        case Right(expr)  => Eval.eval(expr)
    }

  @scala.annotation.tailrec
  def repl(implicit env: Env): Unit = {
    val console = System.console
    val nextLine = console.readLine("ali>")

    readEval(nextLine) match {
      case EnvUpdate(newEnv) =>
        repl(newEnv)
      case result: Result    =>
        println(result.show)
        repl(env)
    }
  }

  repl(Env.root)
}
