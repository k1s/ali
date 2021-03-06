package ali

object Spec {

  object implicits {

    implicit def numericToNum[A](num: A)(implicit ev: Numeric[A]): Num =
      Num(ev.toDouble(num))

    implicit def strToId(str: String): Id = Id(str)

    implicit class EitherOps[A, B](e: Either[A, B]) {
      def get: B = e.toOption.get
    }

  }

  val addLambda =
    Lambda(
      List(Id("x"), Id("y")),
      Apply(
        Id("+"),
        List(Id("x"), Id("y"))))

  val argsStr = "3 4"

  val parsedArgs = List(Num(3), Num(4))

  val addFun = Def(Id("add"), addLambda)

}
