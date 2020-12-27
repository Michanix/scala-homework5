import Utils.{exprToMathML, parseTextField, runJSscript, simplifyExpr}
import scalafx.application.JFXApp
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.VBox
import scalafx.scene.web.WebView

import scala.io.{BufferedSource, Source}

object Main extends JFXApp {
  private val webView: WebView = new WebView()
  private val url: BufferedSource = Source.fromFile("src/main/scala/expression.html")
  private val exprField: TextField = new TextField()
  // Expressions for testing
  private val exampleLabel = new Label("Test examples:")
  private val exmpl1: TextField = new TextField {
    text = "(+ (* (- 5 5) (^ x 2)) 8)"
  }
  private val exmpl2: TextField = new TextField {
    text = "(* 0(+ x 1))"
  }
  private val exmpl3: TextField = new TextField {
    text = "(* 2 (+ x 1))"
  }
  private val exmpl4: TextField = new TextField {
    text = "(/ (^ 2(* 1 x)) (- 4 1))"
  }
  private val exmpl5: TextField = new TextField {
    text = "(* (^ x 7) (^ x 6))"
  }
  private val exmpl6: TextField = new TextField {
    text = "(/ (^ x 4) (^ x 2))"
  }

  private val examplesList: List[TextField] = List(exmpl1, exmpl2,
    exmpl3, exmpl4, exmpl5, exmpl6)
  examplesList.map(l => {
    l.setDisable(false)
    l.setEditable(false)
  })

  private val btn: Button = new Button {
    text = "Simplify"
    onAction = _ => {
      val exprTree = parseTextField(exprField.getText)

      if (exprTree != EmptyExprTree) {
        val simplifiedExpr = simplifyExpr(exprTree)
        val result = exprToMathML(exprTree, simplifiedExpr)
        println(result)
        webView.getEngine.executeScript(runJSscript(result))
      } else {
        println(exprTree)
        webView.getEngine.executeScript(
          "document.getElementById('expression').innerHTML = 'Woops! That\\'s not nice! Maybe check your expression?';"
        )
      }
    }
  }

  private val examples = new VBox {
    children = examplesList
    spacing = 5
    prefWidth = 10
    maxWidth = 150
  }
  private val content =
    new VBox(exprField, btn, exampleLabel, examples, webView) {
    padding = Insets(10)
    spacing = 10
    alignment = Pos.TopLeft
  }

  // loading expression.html into WebView
  webView.getEngine.loadContent(url.mkString)
  stage = new JFXApp.PrimaryStage {
    title = "Expression simplification"
    width  = 400
    height = 500
    minHeight = 500
    minWidth = 400
    scene = new Scene(content)
  }
}
