package ali

import org.scalatest.{Matchers, WordSpec}

class EvalSpec extends WordSpec with Matchers {

  import Spec._
  import Spec.implicits._

  implicit val env = Env.root

  val evaluationOf: Expr => Expr = Eval.evalExpression(_).get

  "Eval" should {

    "eval simple expressions" in {
      evaluationOf(Apply("+", parsedArgs)) shouldEqual Num(7)

      val severalArgs = parsedArgs :+ Num(2)

      evaluationOf(Apply("+", severalArgs)) shouldEqual Num(9)

      evaluationOf(Apply("-", severalArgs)) shouldEqual Num(-3)

      evaluationOf(Apply("*", severalArgs)) shouldEqual Num(24)

      evaluationOf(Apply("/", severalArgs)) shouldEqual Num(0.375)
    }

    "eval compound expressions" in {
      val expr = Apply("+", parsedArgs)

      evaluationOf(Apply("+", List(expr, expr, expr))) shouldEqual Num(21)
    }

    "eval lambda" in {
      evaluationOf(Apply(addLambda, parsedArgs)) shouldEqual Num(7)

      evaluationOf(Apply(addLambda, List(Apply("+", parsedArgs), 7))) shouldEqual Num(14)
    }

    "eval vecs" in {
      evaluationOf(Apply("+", List(Vec(1), Vec(2, 3)))) shouldEqual Vec(1, 2, 3)
    }

    "eval funs" in {
      val eval =
        for {
          e0 <- Eval.eval(addFun)
          e1 <- Eval.eval(Fun("lala", 3))(e0)
          e2 <- Eval.eval(Fun("jopa", 4))(e1)
          e3 <- Eval.eval(Apply("add", List("lala", "jopa")))(e2)
        } yield e3

      eval shouldEqual Result(Right(7))
    }

    "eval predicates" in {
      evaluationOf(Apply("=", List(3, 3))) shouldEqual Bool(true)
      evaluationOf(Apply(">", List(4, 3))) shouldEqual Bool(true)
      evaluationOf(Apply("<", List(2, 3))) shouldEqual Bool(true)

      evaluationOf(Apply("=", List(3, 3, 3))) shouldEqual Bool(true)
      evaluationOf(Apply(">", List(4, 3, 2))) shouldEqual Bool(true)
      evaluationOf(Apply("<", List(2, 3, 4))) shouldEqual Bool(true)
    }

  }

}
