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

  it should "parse vecs" in {
    Parser.parse(s"[]") shouldEqual Vec(Vector())

    Parser.parse(s"[3]") shouldEqual Vec(Vector(Num(3)))

    Parser.parse(s"[3 42]") shouldEqual Vec(Vector(Num(3), Num(42)))

    Parser.parse(s"[a b]") shouldEqual Vec(Vector(Id("a"), Id("b")))

    Parser.parse(s"[[a b] c]") shouldEqual Vec(Vector(Vec(Vector(Id("a"), Id("b"))), Id("c")))

    Parser.parse(s"[$lambdaStr $lambdaStr]") shouldEqual Vec(Vector(parsedLambda, parsedLambda))
  }

}
