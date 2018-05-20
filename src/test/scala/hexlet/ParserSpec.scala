package hexlet

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  it should "parse simple expressions" in {
    Parser.parse("(+ 1 2)") shouldEqual Apply(Id("+"), Seq(Num(1), Num(2)))

    Parser.parse("(- 1 2)") shouldEqual Apply(Id("-"), Seq(Num(1), Num(2)))

    Parser.parse("(* 1 2)") shouldEqual Apply(Id("*"), Seq(Num(1), Num(2)))

    Parser.parse("(/ 1 2)") shouldEqual Apply(Id("/"), Seq(Num(1), Num(2)))

    Parser.parse("(\\ x y (+ x y))") shouldEqual Fun(
      List(Id("x"), Id("y")),
      Apply(
        Id("+"),
        List(Id("x"), Id("y"))))
  }

}
