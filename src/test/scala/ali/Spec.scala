package ali

object Spec {

  object implicits {

    implicit def numericToNum[N](num: N)(implicit ev: Numeric[N]): Num =
      Num(ev.toDouble(num))

    implicit def strToId(str: String): Id = Id(str)

  }

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
