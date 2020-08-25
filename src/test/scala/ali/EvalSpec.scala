package ali

import org.scalatest.{Matchers, WordSpec}

class EvalSpec extends WordSpec with Matchers {

  import Spec._
  import Spec.implicits._

  implicit val env = Env.root

  val evaluationOf: Expr => Expr = Eval.evalExpression(_) match {
    case Left(er) => throw new RuntimeException(er)
    case Right(ok) => ok
  }

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
      Eval.eval(addFun).let(env =>
        Eval.eval(Def("lala", 3))(env).let( env =>
         Eval.eval(Def("jopa", 4))(env).let( env =>
           Eval.eval(Apply("add", List("lala", "jopa")))(env)
      ))) shouldEqual Result(Right(7))
    }

    "eval predicates" in {
      evaluationOf(Apply("=", List(3, 3))) shouldEqual Bool(true)
      evaluationOf(Apply(">", List(4, 3))) shouldEqual Bool(true)
      evaluationOf(Apply("<", List(2, 3))) shouldEqual Bool(true)

      evaluationOf(Apply("=", List(3, 3, 3))) shouldEqual Bool(true)
      evaluationOf(Apply(">", List(4, 3, 2))) shouldEqual Bool(true)
      evaluationOf(Apply("<", List(2, 3, 4))) shouldEqual Bool(true)
    }

    "eval closures" in {
      val closure =
        Apply(
          Lambda(List("x"), Lambda(List("y"), Apply("+",List("x", "y")))),
          List(Num(2.0), Num(3.0))
        )

      evaluationOf(closure) shouldEqual Num(5)
    }

  }

}
