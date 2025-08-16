import scalafx.scene.control._
import javafx.scene.input.MouseButton
import model.GameCell
import scalafx.scene.layout.GridPane


class GameView(controller: GameController, onGameOver: () => Unit, onGameWin: () => Unit, gameCommit: GameCommit) {

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
    val current = Game.get
    val cell    = controller.getCell(current, row, col)

    val (nextState, isOver, isWinNow) = button match {
      case MouseButton.PRIMARY =>
        val s1 = controller.incrementClickCount(current)
        if (cell.isFlagged) {
          (s1, false, controller.isWin(s1))
        } else if (cell.isMine) {
          val s2 = controller.revealAllMines(s1)
          (s2, true, false)
        } else {
          val s2 = controller.revealCellAndNeighbors(s1, row, col)
          (s2, false, controller.isWin(s2))
        }

      case MouseButton.SECONDARY =>
        val s1 =
          if (!cell.isRevealed) controller.toggleFlag(current, row, col)
          else current
        (s1, false, controller.isWin(s1))

      case _ =>
        (current, false, controller.isWin(current))
    }

    // Updating Game to next state
    //gameCommit.update((_, _) => nextState, _ => isOver)
    Game.set(nextState)


    updateView(controller.getGrid(nextState), isGameOver = isOver)

    if (isOver) onGameOver()
    else if (isWinNow) onGameWin()

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
      } else {
        button.disable = false
        button.text = ""
        button.style = ""
      }


      if (isGameOver) button.disable = true
    }
  }

  def markSuggestedMove(row: Int, col: Int): Unit = {
    buttons(row)(col).style = bgYellow
  }


}
