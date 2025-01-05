import scalafx.scene.control._
import scalafx.scene.layout.{Background, GridPane, VBox}


class GameView(controller: GameController) {

  private var buttonGrid: Array[Array[Button]] = Array.ofDim[Button](0, 0)

  def createGrid(rows: Int, columns: Int): GridPane = {

    buttonGrid = Array.ofDim[Button](rows, columns)

    val gridPane = new GridPane {
      hgap = 5
      vgap = 5
    }

    for (row <- 0 until rows; col <- 0 until columns) {

      val button = new Button {
        prefWidth = 40
        prefHeight = 40
        text = ""
        disable = false
        onAction = _ => handleCellClick(row, col)
      }

      buttonGrid(row)(col) = button
      gridPane.add(button, col, row)
    }

    gridPane
  }

  def handleCellClick(row: Int, col: Int): Unit = {

    val isMine = controller.checkIsMine(row, col)

    if(isMine) {
      buttonGrid(row)(col).style = "-fx-background-color: red; -fx-border-color: red; -fx-border-width: 2;"
    } else {
      buttonGrid(row)(col).text = controller.getMineCount(row, col) + ""
      buttonGrid(row)(col).style = "-fx-background-color: lightblue; -fx-border-color: darkblue; -fx-border-width: 2;"
    }

    buttonGrid(row)(col).disable = true
    buttonGrid(row)(col).scene().getRoot.requestFocus()
  }

  def resetGrid(): Unit = {
    buttonGrid.foreach(_.foreach { button =>
      button.text = ""
      button.disable = false
      button.style = ""
    })
  }

}
