object Utils {
  def parseTextField(str: String): ExprTree = {
    /*
    Parse string from TextField to ExprTree
     */
    val expr = ExprParser.run(str)
    expr match {
      case EmptyExprTree => EmptyExprTree
      case _ => expr
    }
  }
  def simplifyExpr(expr: ExprTree): ExprTree = ExprSimplifier.simplify(expr)

  def exprToMathML(originalExpr: ExprTree, simpleExpr: ExprTree): String =
    // Takes two ExprTrees and turn into MathMl code block
    s"${ExprParser.exprToMathMl(originalExpr)}<mo>=</mo>${ExprParser.exprToMathMl(simpleExpr)}"

  def runJSscript(expr: String): String = {
    /*
    JavaScript code block that is executes MathJax in WebView
     */
    val mathMLBlock =
      s"""
         |<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
         |$expr
         |</math>
         |""".stripMargin
    val result = s"""
                    |var out = document.getElementById('expression');
                    |out.innerHTML = '';
                    |out.appendChild(MathJax.mathml2chtml('$mathMLBlock'));
                    |MathJax.typeset();
                    |""".stripMargin
    result.stripLineEnd.replaceAll("\n", "")
  }
}
