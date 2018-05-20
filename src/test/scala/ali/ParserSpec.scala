package ali

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  import Spec._

  it should "parse simple expressions" in {
    Parser.parse(s"(+ $argsStr)") shouldEqual Apply(Id("+"), parsedArgs)

    Parser.parse(s"(- $argsStr)") shouldEqual Apply(Id("-"), parsedArgs)

    Parser.parse(s"(* $argsStr)") shouldEqual Apply(Id("*"), parsedArgs)

    Parser.parse(s"(/ $argsStr)") shouldEqual Apply(Id("/"), parsedArgs)

    Parser.parse(lambdaStr) shouldEqual parsedLambda

    Parser.parse(s"($lambdaStr $argsStr)") shouldEqual Apply(parsedLambda, parsedArgs)
  }

}
