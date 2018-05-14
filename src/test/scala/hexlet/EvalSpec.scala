package hexlet

import org.scalatest.{FlatSpec, Matchers}

class EvalSpec extends FlatSpec with Matchers {

  implicit val env = Environment.root

  it should "eval simple expression" in {

    Eval.eval(Fun(Add, Seq(Num(1), Num(2)))) shouldEqual Num(3)

  }

  it should "eval compound expressions" in {

    val expr = Fun(Add, Seq(Num(1), Num(2)))

    Eval.eval(Fun(Add, Seq(expr, expr, expr))) shouldEqual Num(9)
  }

}
