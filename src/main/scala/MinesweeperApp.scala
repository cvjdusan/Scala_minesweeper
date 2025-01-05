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

    val controller = new GameController()
    val view = new GameView(controller)

    lazy val mainLayout: BorderPane = new BorderPane {
      top = new MenuBar {
        menus = Seq(
          new Menu("Options") {
            items = Seq(
              new MenuItem("New game") {
                onAction = _ => startNewGame(controller, view, difficultyComboBox, levelsListView, mainLayout)
              },
              new MenuItem("Load game") {
                onAction = _ => loadGame(controller, view, mainLayout)
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
      center = new VBox {
        spacing = 10
        alignment = Pos.Center
        children = Seq(
          difficultyComboBox,
          levelsListView
        )
      }
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Minesweeper"
      width = 800
      height = 600
      scene = new Scene {
        root = mainLayout
      }
    }
  }

  private def startNewGame(controller: GameController, view: GameView, difficultyComboBox: ComboBox[String], levelsListView: ListView[String], mainLayout: BorderPane): Unit = {
    val selectedDifficulty = difficultyComboBox.value.value
    val selectedLevel = levelsListView.selectionModel().getSelectedItem
    if (selectedDifficulty != null && selectedLevel != null) {
      if (selectedLevel == "Random") {
        println(s"Starting a new game with difficulty: $selectedDifficulty and random level.")
      } else {
        println(s"Starting a new game with difficulty: $selectedDifficulty and level: $selectedLevel.")
      }
      controller.initializeGame(10, 10)
      val gridPane = view.createGrid(10, 10)
      mainLayout.center = gridPane
    } else {
      new Alert(AlertType.Warning) {
        title = "Warning"
        headerText = "Invalid Selection"
        contentText = "Please select both a difficulty and a level!"
      }.showAndWait()
    }
  }

  private def loadGame(controller: GameController, view: GameView, mainLayout: BorderPane): Unit = {
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

      controller.loadGame(gridData)
      val gridPane = view.createGrid(gridData.length, gridData(0).length)
      mainLayout.center = gridPane
    }
  }

  private def createLevel(): Unit = {
    println("Create level functionality is not implemented yet.")
  }

  private def showResults(): Unit = {
    println("Show results functionality is not implemented yet.")
  }
}

