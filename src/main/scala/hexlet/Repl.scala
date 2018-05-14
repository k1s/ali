package hexlet

object Repl extends App {

  def repl: Unit = {
    implicit val env = Environment.root
    val console = System.console

    while (true) {
      val nextLine = console.readLine("hexlet>")

      val result = Eval.eval(Parser.parse(nextLine))

      println(result)
    }
  }

  repl

}
