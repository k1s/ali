package ali

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  import Spec._
  import Spec.implicits._

  val lambdaStr = """(\x y (+ x y))"""

  it should "parse simple expressions" in {
    Parser.parse(s"(+ $argsStr)").get shouldEqual Apply(Id("+"), parsedArgs)

    Parser.parse(s"(- $argsStr)").get shouldEqual Apply(Id("-"), parsedArgs)

    Parser.parse(s"(* $argsStr)").get shouldEqual Apply(Id("*"), parsedArgs)

    Parser.parse(s"(/ $argsStr)").get shouldEqual Apply(Id("/"), parsedArgs)

    Parser.parse(lambdaStr).get shouldEqual parsedLambda

    Parser.parse(s"($lambdaStr $argsStr)").get shouldEqual Apply(parsedLambda, parsedArgs)
  }

  it should "parse vecs" in {
    Parser.parse(s"[]").get shouldEqual Vec(Vector())

    Parser.parse(s"[3]").get shouldEqual Vec(Vector(Num(3)))

    Parser.parse(s"[3 42]").get shouldEqual Vec(Vector(Num(3), Num(42)))

    Parser.parse(s"[a b]").get shouldEqual Vec(Vector(Id("a"), Id("b")))

    Parser.parse(s"[[a b] c]").get shouldEqual Vec(Vector(Vec(Vector(Id("a"), Id("b"))), Id("c")))

    Parser.parse(s"[$lambdaStr $lambdaStr]").get shouldEqual Vec(Vector(parsedLambda, parsedLambda))
  }

}
