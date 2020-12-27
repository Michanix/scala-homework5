import fastparse._
import fastparse.SingleLineWhitespace._

import scala.runtime.Nothing$

object ExprParser {

  private def expr[_: P]: P[ExprTree] = P((
    /*
    Main parser function(sort of) that parse the whole S-expression
    and return its tree representation.
     */
    "(" ~ binOp ~ atom ~ atom ~/ ")")
    .map(x => x._1 match {
    case "+" => Sum(x._2, x._3)
    case "-" => Sub(x._2, x._3)
    case "*" => Mul(x._2, x._3)
    case "/" => Div(x._2, x._3)
    case "^" => Pow(x._2, x._3)
  }))
  private def binOp[_: P]: P[String] = P("*" | "-" | "/" | "+" | "^").rep(1).!
  private def atom[_: P]: P[ExprTree] = P(expr | number | vars )
  private def number[_: P]: P[Num] = P(CharsWhileIn("0-9").!.map(x => Num(x.toInt)))
  private def vars[_: P]: P[Var] = P(CharIn("a-z").!.map(Var))

  def exprToMathMl(str: ExprTree): String = str match {
    /*
    Takes ExprTree and converts its into MathMl code block
     */
    case Num(n) => s"<mn>$n</mn>"
    case Var(x) => s"<mi>$x</mi>"
    case Sum(v@Var(_), r) =>
      s"<mo>(${exprToMathMl(v)}<mo>+</mo>${exprToMathMl(r)})</mo>"
    case Sum(l, v@Var(_)) =>
      s"<mo>(${exprToMathMl(l)}<mo>+</mo>${exprToMathMl(v)})</mo>"
    case Sum(l, r) => s"${exprToMathMl(l)}<mo>+</mo>${exprToMathMl(r)}"

    case Sub(v@Var(_), r) =>
      s"<mo>(${exprToMathMl(v)}<mo>-</mo>${exprToMathMl(r)})</mo>"
    case Sub(l, v@Var(_)) =>
      s"<mo>(${exprToMathMl(l)}<mo>-</mo>${exprToMathMl(v)})</mo>"
    case Sub(l, r) => s"${exprToMathMl(l)}<mo>-</mo>${exprToMathMl(r)}"

    case Mul(v@Var(_), r) =>
      s"<mo>(${exprToMathMl(v)}<mo>&times;</mo>${exprToMathMl(r)})</mo>"
    case Mul(l, v@Var(_)) =>
      s"<mo>(${exprToMathMl(l)}<mo>&times;</mo>${exprToMathMl(v)})</mo>"
    case Mul(n1@Num(_), n2@Num(_)) =>
      s"<mo>(${exprToMathMl(n1)}<mo>&times;</mo>${exprToMathMl(n2)})</mo>"
    case Mul(l, r) => s"${exprToMathMl(l)}<mo>&times;</mo>${exprToMathMl(r)}"

    case Div(l, r) => s"<mfrac><mrow>${exprToMathMl(l)}</mrow><mrow>${exprToMathMl(r)}</mrow></mfrac>"
    case Pow(l, r) => s"<msup>${exprToMathMl(l)}${exprToMathMl(r)}</msup>"
  }
  def run(str: String): ExprTree = {
    /*
    Runs FastParse parse() method
    on a given string. On success returns result of type ExprTree.
     */
    val result = parse(str, expr(_), verboseFailures = true)
    result match {
      case Parsed.Success(value, _) => value
      case _: Parsed.Failure => EmptyExprTree
    }
  }
}