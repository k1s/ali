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

  it should "parse long names" in {
    val d = "dvigai"
    val j = "jopoi"

    val str = s"(+ $d $j)"
    val expected = Apply(Id("+"), List(Id(d), Id(j)))

    Parser.parse(str).get shouldEqual expected

    val lstr = s"(\\$d $j (+ $d $j))"
    val lexpected = Lambda(List(Id(d), Id(j)), expected)

    Parser.parse(lstr).get shouldEqual lexpected
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
