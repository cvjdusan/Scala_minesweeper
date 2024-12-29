import scalafx.application.JFXApp3
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.layout.{StackPane, VBox}

object MinesweeperApp extends JFXApp3 {
  override def start(): Unit = {
    val gridSize = 10
    val numMines = 10

    val controller = new GameController(gridSize, numMines)

    controller.initializeGame()

    val view = new GameView(controller, gridSize)

    stage = new JFXApp3.PrimaryStage {
      title = "Minesweeper"
      width = 800
      height = 600
      scene = new Scene {
        root = new StackPane  {
          children = Seq(
            view.createGrid()
          )
          style = "-fx-padding: 20;"
        }
      }
    }
  }
}
