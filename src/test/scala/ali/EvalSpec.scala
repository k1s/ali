package ali

import org.scalatest.{FlatSpec, Matchers}

class EvalSpec extends FlatSpec with Matchers {

  import Spec._

  implicit val env = Env.root

  it should "eval simple expressions" in {
    Eval.eval(Apply(Id("+"), parsedArgs)) shouldEqual Num(7)

    val severalArgs = parsedArgs :+ Num(2)

    Eval.eval(Apply(Id("+"), severalArgs)) shouldEqual Num(9)

    Eval.eval(Apply(Id("-"), severalArgs)) shouldEqual Num(-3)

    Eval.eval(Apply(Id("*"), severalArgs)) shouldEqual Num(24)

    Eval.eval(Apply(Id("/"), severalArgs)) shouldEqual Num(0.375)
  }

  it should "eval compound expressions" in {
    val expr = Apply(Id("+"), parsedArgs)

    Eval.eval(Apply(Id("+"), Seq(expr, expr, expr))) shouldEqual Num(21)
  }

  it should "eval lambda" in {
    Eval.eval(Apply(parsedLambda, parsedArgs)) shouldEqual Num(7)

    Eval.eval(Apply(parsedLambda, Seq(Apply(Id("+"), parsedArgs), Num(7)))) shouldEqual Num(14)
  }

}
