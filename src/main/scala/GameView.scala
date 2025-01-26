import scalafx.scene.control._
import javafx.scene.input.MouseButton
import scalafx.scene.layout.GridPane


class GameView(controller: GameController, onGameOver: () => Unit) {

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
        onMousePressed  = event => handleCellClick(row, col, event.getButton)
      }

      buttonGrid(row)(col) = button
      gridPane.add(button, col, row)
    }

    gridPane
  }


  private def handleCellClick(row: Int, col: Int, button: MouseButton): Unit = {

    val cell = controller.getCell(row, col)

    button match {
      case MouseButton.PRIMARY =>
        controller.incrementClickCount()
        if(cell.isMine) {
          // reveal all
          controller.revealAllMines()
          updateView(controller.getGrid(), isGameOver = true)
          onGameOver()
        } else {
          // reveal cell and neighbours without mines
          controller.revealCell(row, col)
          updateView(controller.getGrid(), isGameOver = false)
        }


      case MouseButton.SECONDARY =>
        val currentButton = buttonGrid(row)(col)
        if (!cell.isRevealed) {
          if (Option(currentButton.text.value).getOrElse("") == "🚩") {
            currentButton.text = ""
          } else {
            currentButton.text = "🚩"
          }
        }

      case _ =>
    }


    buttonGrid(row)(col).scene().getRoot.requestFocus()
  }

  def updateView(grid: Array[Array[GameCell]], isGameOver: Boolean): Unit = {
    for (row <- grid.indices; col <- grid(row).indices) {
      val cell = grid(row)(col)
      val button = buttonGrid(row)(col)

      if (cell.isRevealed) {
        button.disable = true
        if (cell.isMine) {
          button.text = "💣"
          button.style = "-fx-background-color: red;"
        } else if (cell.adjacentMines > 0) {
          button.text = cell.adjacentMines.toString
          button.style = "-fx-background-color: lightblue;"
        } else {
          button.text = ""
          button.style = "-fx-background-color: grey;"
        }
      }

      if(isGameOver) button.disable = true
    }
  }


  def resetGrid(): Unit = {
    buttonGrid.foreach(_.foreach { button =>
      button.text = ""
      button.disable = false
      button.style = ""
    })
  }

  def markSuggestedMove(row: Int, col: Int): Unit = {
    buttonGrid(row)(col).style = "-fx-background-color: yellow;"
  }



}
