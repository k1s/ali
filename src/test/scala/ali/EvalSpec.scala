package ali

import org.scalatest.{FlatSpec, Matchers}

class EvalSpec extends FlatSpec with Matchers {

  import Spec._
  import Spec.implicits._

  implicit val env = Env.root

  it should "eval simple expressions" in {
    Eval.eval(Apply(Id("+"), parsedArgs)).get shouldEqual Num(7)

    val severalArgs = parsedArgs :+ Num(2)

    Eval.eval(Apply(Id("+"), severalArgs)).get shouldEqual Num(9)

    Eval.eval(Apply(Id("-"), severalArgs)).get shouldEqual Num(-3)

    Eval.eval(Apply(Id("*"), severalArgs)).get shouldEqual Num(24)

    Eval.eval(Apply(Id("/"), severalArgs)).get shouldEqual Num(0.375)
  }

  it should "eval compound expressions" in {
    val expr = Apply(Id("+"), parsedArgs)

    Eval.eval(Apply(Id("+"), Seq(expr, expr, expr))).get shouldEqual Num(21)
  }

  it should "eval lambda" in {
    Eval.eval(Apply(parsedLambda, parsedArgs)).get shouldEqual Num(7)

    Eval.eval(Apply(parsedLambda, Seq(Apply("+", parsedArgs), 7))).get shouldEqual Num(14)
  }

  it should "eval vecs" in {
    Eval.eval(Apply("+", Seq(Vec(1), Vec(2, 3)))).get shouldEqual Vec(1, 2, 3)
  }

}