package hexlet


object Eval {

  def eval(expr: Expr)(implicit env: Environment): Expr =
    expr match {
      case Fun(fun, args) =>
        fun match {
          case name: Name =>
            applyF(name, args.map(eval))
          case _ =>
            ???
        }
      case _ =>
        expr
    }

  def applyF(name: Name, args: Seq[Expr])(implicit env: Environment): Expr =
    if (args.size == 1)
      eval(args.head)
    else
      env.get(name)(args)

}
