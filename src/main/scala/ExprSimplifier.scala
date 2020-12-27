import scala.math.pow

object ExprSimplifier {
  private def eval(tree: ExprTree): Int = tree match {
    /*
    Evaluates arithmetical operations of ExprTree and returns result.
     */
    case Num(n)    => n
    case Sum(l, r) => eval(l) + eval(r)
    case Sub(l, r) => eval(l) - eval(r)
    case Mul(l, r) => eval(l) * eval(r)
    case Div(l, r) => eval(l) / eval(r)
    case Pow(l, r) => pow(eval(l), eval(r)).toInt
  }

  def simplify(expr: ExprTree, recursive: Boolean = true): ExprTree = expr match {
    /*
    Main expression simplifying function.
    Reason behind 'recursive' argument:
    https://stackoverflow.com/a/26616100
     */
    case n@Num(_) => n
    case v@Var(_) => v
    // Sum
    case Sum(l, Num(0)) => simplify(l)
    case Sum(Num(0), r) => simplify(r)
    case s@Sum(Num(_), Num(_)) => Num(eval(s))
    case s@Sum(Num(_), Var(_)) => s
    case s@Sum(Var(_), Num(_)) => s
    case Sum(v1@Var(_), v2@Var(_)) if v1 == v2 => Var(s"2$v1")
    // Sum special cases
    case Sum(n1@Num(_), Sum(v@Var(_), ll)) =>
      simplify(Sum(v, simplify(Sum(n1, simplify(ll)))))
    case Sum(n1@Num(_), Sum(rr, v@Var(_))) =>
      simplify(Sum(v, simplify(Sum(n1, simplify(rr)))))
    case Sum(n1@Num(_), Sub(n2@Num(_), rr)) =>
      simplify(Sub(Sum(n1, n2), simplify(rr)))
    case Sum(n1@Num(_), Sub(ll, n2@Num(_))) =>
      simplify(Sub(Sum(n1, n2), simplify(ll)))
    case Sum(l, r) if recursive =>
      simplify(Sum(simplify(l), simplify(r)), recursive = false)
    // Sub
    case Sub(l, Num(0)) => simplify(l)
    case Sub(Num(0), r) => simplify(r)
    case sub@Sub(Num(_), Num(_)) => Num(eval(sub))
    case sub@Sub(Num(_), Var(_)) => sub
    case sub@Sub(Var(_), Num(_)) => sub
    case Sub(v1@Var(_), v2@Var(_)) if v1 == v2 => Num(0)
    // Sub special cases
    case Sub(n1@Num(_), Sub(v@Var(_), rr)) =>
      simplify(Sub(v, simplify(Sub(n1, simplify(rr)))))
    case Sub(n1@Num(_), Sub(ll, v@Var(_))) =>
      simplify(Sub(v, simplify(Sub(n1, simplify(ll)))))
    case Sub(n1@Num(_), Sum(n2@Num(_), rr)) =>
      simplify(Sum(Sub(n1, n2), simplify(rr)))
    case Sub(n1@Num(_), Sum(ll, n2@Num(_))) =>
      simplify(Sum(Sub(n1, n2), simplify(ll)))
    case Sub(l, r) if recursive =>
      simplify(Sub(simplify(l), simplify(r)), recursive = false)
    // Multiplication
    case Mul(l, Num(1)) => simplify(l)
    case Mul(Num(1), r) => simplify(r)
    case Mul(_, Num(0)) => Num(0)
    case Mul(Num(0), _) => Num(0)
    case mult@Mul(Num(_), Num(_)) => Num(eval(mult))
    case Mul(v1@Var(_), v2@Var(_)) if v1 == v2 => Pow(v1, Num(2))
    // Mult special cases
    case Mul(Pow(v1@Var(_), n1@Num(_)), Pow(v2@Var(_), n2@Num(_))) if v1 == v2 =>
      Pow(v1, simplify(Sum(n1, n2)))
    case Mul(l, r) if recursive =>
      simplify(Mul(simplify(l), simplify(r)), recursive = false)
    // Division
    case Div(l, Num(1)) => simplify(l)
    case Div(Num(0), _) => Num(0)
    case Div(_, Num(0)) => Num(0)
    case div @ Div(Num(_), Num(_)) => Num(eval(div))
    case Div(v1@Var(_), v2@Var(_)) if v1 == v2 => Num(1)
    // Division special cases
    case Div(Pow(v1@Var(_), n1@Num(_)), Pow(v2@Var(_), n2@Num(_))) if v1 == v2 =>
      Pow(v1, simplify(Sub(n1, n2)))
    case Div(l, r) if recursive =>
      simplify(Div(simplify(l), simplify(r)), recursive = false)
    // Power
    case Pow(l, Num(1)) => simplify(l)
    case Pow(Num(1), _) => Num(1)
    case Pow(_, Num(0)) => Num(1)
    case p @ Pow(Num(_), Num(_)) => Num(eval(p))
    // Power special cases
    case Pow(Mul(v1@Var(_), v2@Var(_)), r) =>
      simplify(Mul(Pow(v1, simplify(r)), Pow(v2, simplify(r))))
    case Pow(l, r) if recursive =>
      simplify(Pow(simplify(l), simplify(r)), recursive = false)
    // If no match, just return what it is
    case _ => expr
  }
}
