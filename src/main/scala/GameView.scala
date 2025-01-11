import scalafx.scene.control._
import scalafx.scene.layout.{Background, GridPane, VBox}

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
      controller.revealAllMines()
      updateView(controller.getGrid(), isGameOver = true)
      onGameOver()
      // reveal all
    } else {
      // reveal cell and neighbours without mines
      controller.revealCell(row, col)
      updateView(controller.getGrid(), isGameOver = false)
    }



    buttonGrid(row)(col).disable = true
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



}
