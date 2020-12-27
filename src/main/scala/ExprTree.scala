sealed trait ExprTree

case object EmptyExprTree extends ExprTree
case class Num(n: Int) extends ExprTree {
  override def toString: String = n.toString
}
case class Var(x: String) extends ExprTree {
  override def toString: String = x
}
case class Sum(l: ExprTree, r: ExprTree) extends ExprTree {
  override def toString: String = s"($l+$r)"
}
case class Sub(l: ExprTree, r: ExprTree) extends ExprTree {
  override def toString: String = s"($l-$r)"
}
case class Mul(l: ExprTree, r: ExprTree) extends ExprTree {
  override def toString: String = s"$l*$r"
}
case class Div(l: ExprTree, r: ExprTree) extends ExprTree {
  override def toString: String = s"$l/$r"
}
case class Pow(l: ExprTree, r: ExprTree) extends ExprTree {
  override def toString: String = s"$l^$r"
}