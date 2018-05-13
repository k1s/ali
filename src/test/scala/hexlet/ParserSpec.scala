package hexlet

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  it should "parse simple expression" in {

    Parser.parse("( + 1 2)") shouldEqual Fun(Add, Seq(Num(1), Num(2)))

  }

}
