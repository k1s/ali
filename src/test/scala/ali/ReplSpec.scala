package ali

import org.scalatest.{Matchers, WordSpec}

class ReplSpec extends WordSpec with Matchers {

  implicit val env = Env.root

  def runOf(command: String)(implicit env: Env): Expr =
    Repl.readEval(command)(env) match {
      case Result(Right(ok)) => ok
      case other => throw new RuntimeException(other.toString)
    }

  def errorOf(command: String)(implicit env: Env): String =
    Repl.readEval(command)(env) match {
      case Result(Left(er)) => er
      case other => throw new RuntimeException(other.toString)
    }


  "Interpreter" should {

    "be fine" in {
      Repl.readEval("(+ 2 3 4 5)") shouldEqual Result(Right(Num(14.0)))

      Repl.readEval("(_ 23r sfg)") shouldEqual Result(Left("_ is not defined!"))
    }

    "do ifs" in {
      runOf("(if (> 2 3) 14 15)") shouldEqual Num(15.0)
      runOf("(if (< 2 3) 41 51)") shouldEqual Num(41.0)
      errorOf("(if (+ 2 13) 4 5)") should include ("Num(15.0)")
    }

    "do lazy ifs" in {
      val withP = Repl.readEval("(def p (\\x (p x)))").asInstanceOf[EnvUpdate].env

      runOf("(if (> 10 0) 0 (p 42))")(withP) shouldEqual Num(0.0)
    }

  }

}

