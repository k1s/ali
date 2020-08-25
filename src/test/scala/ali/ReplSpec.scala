package ali

import org.scalatest.{Matchers, WordSpec}

class ReplSpec extends WordSpec with Matchers {

  implicit val env = Env.root

  def runOf(command: String)(implicit env: Env): Expr =
    Repl.readEval(command)(env) match {
      case Result(Right(ok)) => ok
      case other => throw new RuntimeException(other.toString)
    }

  def envOf(command: String)(implicit env: Env): Env =
    Repl.readEval(command)(env) match {
      case EnvUpdate(env) => env
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

      Repl.readEval("(_ 23r sfg)") shouldEqual Result(Left("_ is not defined in env"))
    }

    "do ifs" in {
      runOf("(if (> 2 3) 14 15)") shouldEqual Num(15.0)
      runOf("(if (< 2 3) 41 51)") shouldEqual Num(41.0)
      errorOf("(if (+ 2 13) 4 5)") should include ("Num(15.0)")
    }

    "do lazy ifs" in {
      val withP = envOf("(def p (\\x (p x)))")

      runOf("(if (> 10 0) 0 (p 42))")(withP) shouldEqual Num(0.0)
    }

    "run recursive funs" in {
      val factorial = envOf("(def fac (\\n acc (if (= 1 n) acc (fac (- n 1) (* n acc)))))")

      runOf("(fac 5 1)")(factorial) shouldEqual Num(120.0)
    }

//    "TODO run tail recursive funs" ignore not implemented yet {
//      val factorial = Repl.readEval("(def fac (fn [n acc] (if (= 1 n) acc (recur (- n 1) (* n acc)))))").asInstanceOf[EnvUpdate].env
//
//      runOf("(fac 5000 1)")(factorial) shouldEqual Num(120.0)
//    }
//
  }

}

