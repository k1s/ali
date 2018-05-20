package ali


object Eval {

  def eval(expr: Expr)(implicit env: Env): Expr = {
    println(s"expr $expr")
    expr match {
      case Apply(fun, args) =>
        fun match {
          case name: Id =>
            println(s"apply name $fun $args")
            applyF(name, args.map(eval))
          case Lambda(lambdaArgs, body) =>
            println(s"apply lambda $fun $args")
            //todo check arity
            val lambdaEnv = lambdaArgs.map(_.id).zip(args.map(Expr.lift)).toMap
            eval(body)(Env(env, lambdaEnv))
        }
      case n: Num =>
        println(s"num $n")
        n
      case Id(id) =>
        println(s"id $id")
        eval(env.get(id).const)
    }
  }

  def applyF(name: Id, args: Seq[Expr])(implicit env: Env): Expr = {
    println(s"apply f name $name")
    println(s"apply f args $args")

    if (args.size == 1)
      eval(args.head)
    else
      env.get(name.id)(args)
  }

}
