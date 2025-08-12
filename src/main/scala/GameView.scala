import scalafx.scene.control._
import javafx.scene.input.MouseButton
import model.GameCell
import scalafx.scene.layout.GridPane


class GameView(controller: GameController, onGameOver: () => Unit, onGameWin: () => Unit) {

  private val bgYellow = "-fx-background-color: yellow;"
  private val bgRed = "-fx-background-color: red;"
  private val bgLightBlue = "-fx-background-color: lightblue;"
  private val bgGrey = "-fx-background-color: grey;"

  private var buttons: Vector[Vector[Button]] = Vector.empty

  def createGrid(rows: Int, columns: Int): GridPane = {
    buttons = Vector.tabulate(rows, columns) { (r, c) =>
      new Button {
        prefWidth = 40
        prefHeight = 40
        text = ""
        onMousePressed = event => handleCellClick(r, c, event.getButton)
      }
    }

    val gridPane = new GridPane {
      hgap = 5
      vgap = 5
    }

    for (row <- 0 until rows; col <- 0 until columns) {
      gridPane.add(buttons(row)(col), col, row)
    }

    gridPane
  }

  private def handleCellClick(row: Int, col: Int, button: MouseButton): Unit = {
    val currentState = Game.get
    val cell = controller.getCell(currentState, row, col)

    button match {
      case MouseButton.PRIMARY =>
        val newState = controller.incrementClickCount(currentState)
        Game.set(newState)

        if(cell.isFlagged) {
          // do nothing
        }
        else if (cell.isMine) {
          val finalState = controller.revealAllMines(currentState)
          Game.set(finalState)
          updateView(controller.getGrid(finalState), isGameOver = true)
          onGameOver()
        } else {
          val newState = controller.revealCellAndNeighbors(currentState, row, col)
          Game.set(newState)
          updateView(controller.getGrid(newState), isGameOver = false)
          if (controller.isWin(newState)) {
            onGameWin()
          }
        }

      case MouseButton.SECONDARY =>
        val currentButton = buttons(row)(col)
        if (!cell.isRevealed) {
          if (Option(currentButton.text.value).getOrElse("") == "🚩") {
            currentButton.text = ""
          } else {
            currentButton.text = "🚩"
          }
          val newState = controller.toggleFlag(currentState, row, col)
          Game.set(newState)
        }

      case _ =>
    }

    buttons(row)(col).scene().getRoot.requestFocus()
  }

  def updateView(grid: Vector[Vector[GameCell]], isGameOver: Boolean): Unit = {
    for (row <- grid.indices; col <- grid(row).indices) {
      val cell = grid(row)(col)
      val button = buttons(row)(col)

      if (cell.isRevealed) {
        button.disable = true
        if (cell.isMine) {
          button.text = "💣"
          button.style = bgRed
        } else if (cell.adjacentMines > 0) {
          button.text = cell.adjacentMines.toString
          button.style = bgLightBlue
        } else {
          button.text = ""
          button.style = bgGrey
        }
      } else if(cell.isFlagged) {
        button.text = "🚩"
      }

      if (isGameOver) button.disable = true
    }
  }

  def markSuggestedMove(row: Int, col: Int): Unit = {
    buttons(row)(col).style = bgYellow
  }

  def isGameLost(state: GameState): Boolean = {
    state.grid.flatten.exists(cell => cell.isRevealed && cell.isMine)
  }

}
