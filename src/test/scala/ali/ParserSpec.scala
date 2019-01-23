package ali

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  import Spec._
  import Spec.implicits._

  val lambdaStr = """(\x y (+ x y))"""

  val parse: String => Expr = Parser.parse(_).get

  it should "parse simple expressions" in {
    parse(s"(+ $argsStr)") shouldEqual Apply(Id("+"), parsedArgs)

    parse(s"(- $argsStr)") shouldEqual Apply(Id("-"), parsedArgs)

    parse(s"(* $argsStr)") shouldEqual Apply(Id("*"), parsedArgs)

    parse(s"(/ $argsStr)") shouldEqual Apply(Id("/"), parsedArgs)

    parse(lambdaStr) shouldEqual addLambda

    parse(s"($lambdaStr $argsStr)") shouldEqual Apply(addLambda, parsedArgs)
  }

  it should "parse long names" in {
    val d = "dvigai"
    val j = "jopoi"

    val str = s"(+ $d $j)"
    val expected = Apply(Id("+"), List(Id(d), Id(j)))

    parse(str) shouldEqual expected

    val lstr = s"(\\$d $j (+ $d $j))"
    val lexpected = Lambda(List(Id(d), Id(j)), expected)

    parse(lstr) shouldEqual lexpected
  }

  it should "parse vecs" in {
    parse(s"[]") shouldEqual Vec(Vector())

    parse(s"[3]") shouldEqual Vec(Vector(Num(3)))

    parse(s"[3 42]") shouldEqual Vec(Vector(Num(3), Num(42)))

    parse(s"[a b]") shouldEqual Vec(Vector(Id("a"), Id("b")))

    parse(s"[[a b] c]") shouldEqual Vec(Vector(Vec(Vector(Id("a"), Id("b"))), Id("c")))

    parse(s"[$lambdaStr $lambdaStr]") shouldEqual Vec(Vector(addLambda, addLambda))
  }

  it should "parse functions" in {
    parse("(fn f1 2)") shouldEqual Fun(Id("f1"), Num(2))
    parse("(fn add (\\x y (+ x y)))") shouldEqual addFun
  }

}
