package hexlet


object Eval {

  def eval(expr: Expr)(implicit env: Environment): Expr =
    expr match {
      case Fun(fun, args) =>
        fun match {
          case name: Name =>
            applyF(name, args)
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
      name match {
        case Add =>
          args.head match {
            case Num(n) =>
              Num(n + applyF(name, args.tail).asInstanceOf[Num].n)
            case expr =>
              Num(applyF(name, Seq(expr)).asInstanceOf[Num].n + applyF(name, args.tail).asInstanceOf[Num].n)
          }
      }

}
