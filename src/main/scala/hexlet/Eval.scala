package hexlet


object Eval {

  def eval(expr: Expr)(implicit env: Environment): Expr =
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            applyF(name, args.map(eval))
          case _ =>
            ???
        }
      case _ =>
        expr
    }

  def applyF(name: Id, args: Seq[Expr])(implicit env: Environment): Expr =
    if (args.size == 1)
      eval(args.head)
    else
      env.get(name.id)(args)

}
