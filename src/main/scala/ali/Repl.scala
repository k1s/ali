package ali

import cats.implicits._
import Expr.implicits._

object Repl extends App {

  lazy val error = "Unexpected error"

  def readEval(nextLine: String)(implicit env: Env) =
    Parser.parse(nextLine).flatMap(Eval.eval) match {
        case Left(l) =>
          s"$error: $l"
        case Right(r) =>
          r.show
      }

  def repl(implicit env: Env) = {
    val console = System.console

    while (true) {
      val nextLine = console.readLine("ali>")
      val result = readEval(nextLine)
      println(result)
    }
  }

  repl(Env.root)
}
