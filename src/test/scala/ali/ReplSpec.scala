package ali

import org.scalatest.{FlatSpec, Matchers}

class ReplSpec extends FlatSpec with Matchers {

  implicit val env = Env.root

  it should "be fine" in {
    Repl.readEval("(+ 2 3 4 5)") shouldEqual Result(Right(Num(14.0)))

    Repl.readEval("(_ 23r sfg)") shouldEqual Result(Left("_ is not defined!"))
  }

}

