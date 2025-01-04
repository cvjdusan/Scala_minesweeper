import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ComboBox, ListView, Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, StackPane, VBox}
import scalafx.stage.FileChooser

import java.io.{BufferedReader, File, FileReader}


object MinesweeperApp extends JFXApp3 {
  override def start(): Unit = {
    val difficulty = Seq("Beginner", "Normal", "Advanced")
    val levels = Map(
      "Beginner" -> Seq("Level 1", "Level 2", "Random"),
      "Normal" -> Seq("Level 1", "Level 2", "Random"),
      "Advanced" -> Seq("Level 1", "Level 2", "Random")
    )
    val difficultyComboBox = new ComboBox[String] {
      promptText = "Choose difficulty"
      items = ObservableBuffer[String](difficulty: _*)
    }

    val levelsListView = new ListView[String] {
      prefHeight = 100
      disable = true
    }

    difficultyComboBox.onAction = _ => {
      val selectedDifficulty = difficultyComboBox.value.value
      levelsListView.items = ObservableBuffer[String](levels.getOrElse(selectedDifficulty, Seq.empty[String]): _*)
      levelsListView.disable = false
    }

    val gridSize = 10
    val numMines = 10

    val controller = new GameController(gridSize, numMines)

    controller.initializeGame()

    val view = new GameView(controller, gridSize)

    val menuBar = new MenuBar {
      menus = Seq(
        new Menu("Options") {
          items = Seq(
            new MenuItem("New game") {
              onAction = _ => {
                startNewGame(controller, view, difficultyComboBox, levelsListView)              }
            },
            new MenuItem("Load game") {
              onAction = _ => loadGame(controller, view)
            },
            new MenuItem("Create level") {
              onAction = _ => createLevel()
            },
            new MenuItem("Results") {
              onAction = _ => showResults()
            }
          )
        }
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Minesweeper"
      width = 800
      height = 600
      scene = new Scene {
        root = new BorderPane {
          top = menuBar
          center = new VBox {
            spacing = 10
            alignment = Pos.Center
            children = Seq(
              difficultyComboBox,
              levelsListView,
              view.createGrid()
            )
          }
        }
      }
    }
  }

  private def startNewGame(controller: GameController, view: GameView, difficultyComboBox: ComboBox[String], levelsListView: ListView[String]): Unit = {
    val selectedDifficulty = difficultyComboBox.value.value
    val selectedLevel = levelsListView.selectionModel().getSelectedItem
    if (selectedDifficulty != null && selectedLevel != null) {
      if (selectedLevel == "Random") {
        println(s"Starting a new game with difficulty: $selectedDifficulty and random level.")
      } else {
        println(s"Starting a new game with difficulty: $selectedDifficulty and level: $selectedLevel.")
      }
      controller.initializeGame()
      view.resetGrid()
    } else {
      new Alert(AlertType.Warning) {
        title = "Warning"
        headerText = "Invalid Selection"
        contentText = "Please select both a difficulty and a level!"
      }.showAndWait()
    }
  }


  private def loadGame(controller: GameController, view: GameView): Unit = {
    val fileChooser = new FileChooser {
      title = "Load Game"
      extensionFilters.add(new FileChooser.ExtensionFilter("Game Files", "*.txt"))
    }
    val file: File = fileChooser.showOpenDialog(stage)
    if (file != null) {
      val reader = new BufferedReader(new FileReader(file))
      val gridData = reader.lines().toArray.map(_.toString)
      reader.close()

      println("Loaded grid:")
      gridData.foreach(println)

     // controller.loadGame(gridData)
     // view.updateGrid(gridData)
    }
  }


  private def createLevel(): Unit = {

  }


  private def showResults(): Unit = {

  }

}
