package ali

object Spec {

  val lambdaStr = """(\x y (+ x y))"""

  val parsedLambda =
    Lambda(
      List(Id("x"), Id("y")),
      Apply(
        Id("+"),
        List(Id("x"), Id("y"))))

  val argsStr = "3 4"

  val parsedArgs = Seq(Num(3), Num(4))

}
