import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ComboBox, ListView, Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, StackPane, VBox}
import scalafx.stage.FileChooser
import java.io.{BufferedReader, File, FileReader}
import scalafx.scene.control._

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

    val startButton = new Button("Start Game") {
      disable = true
    }

    difficultyComboBox.onAction = _ => {
      val selectedDifficulty = difficultyComboBox.value.value
      levelsListView.items = ObservableBuffer[String](levels.getOrElse(selectedDifficulty, Seq.empty[String]): _*)
      levelsListView.disable = false
      startButton.disable = levelsListView.selectionModel().getSelectedItem == null
    }

    levelsListView.selectionModel().selectedItemProperty().addListener { (_, _, newValue) =>
      startButton.disable = newValue == null
    }

    val controller = new GameController()
    val view = new GameView(controller)

    lazy val mainLayout: BorderPane = new BorderPane {
      top = new MenuBar {
        menus = Seq(
          new Menu("Options") {
            items = Seq(
              new MenuItem("New game") {
                onAction = _ => resetToSelection(difficultyComboBox, levelsListView, startButton, mainLayout)
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
          levelsListView,
          startButton
        )
      }
    }

    startButton.onAction = _ => startNewGame(controller, view, difficultyComboBox, levelsListView, mainLayout)

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

    if (selectedDifficulty == null || selectedLevel == null) {
      new Alert(AlertType.Warning) {
        title = "Warning"
        headerText = "Invalid Selection"
        contentText = "Please select both a difficulty and a level!"
      }.showAndWait()
      return
    }

    var gridData: Array[String] = Array.empty[String]

    selectedDifficulty match {
      case "Beginner" =>
        val filePath = "C:\\Users\\cvdus\\Downloads\\easy_level_1.txt"
        val file = getFile(filePath)
        gridData = readDataFromFile(file.get)

      case "Normal" =>
        val filePath = "C:\\Users\\cvdus\\Downloads\\normal_level_1.txt"
        val file = getFile(filePath)
        gridData = readDataFromFile(file.get)

      case "Advanced" =>
        val filePath = "C:\\Users\\cvdus\\Downloads\\advanced_level_1.txt"
        val file = getFile(filePath)
        gridData = readDataFromFile(file.get)

      case _ =>
        println(s"Unknown difficulty: $selectedDifficulty")
        return
    }


    startGame(controller, view, mainLayout, gridData)
  }


  def getFile(filePath: String): Option[File] = {
    val file = new File(filePath)
    if (file.exists() && file.isFile) {
      Some(file)
    } else {
      println(s"File not found or not valid: $filePath")
      None
    }
  }

  private def resetToSelection(difficultyComboBox: ComboBox[String], levelsListView: ListView[String], startButton: Button, mainLayout: BorderPane): Unit = {
    difficultyComboBox.value = null
    levelsListView.items.value.clear()
    levelsListView.disable = true
    startButton.disable = true
    mainLayout.center = new VBox {
      spacing = 10
      alignment = Pos.Center
      children = Seq(
        difficultyComboBox,
        levelsListView,
        startButton
      )
    }
  }

  private def loadGame(controller: GameController, view: GameView, mainLayout: BorderPane): Unit = {
    val fileChooser = new FileChooser {
      title = "Load Game"
      extensionFilters.add(new FileChooser.ExtensionFilter("Game Files", "*.txt"))
    }
    val file: File = fileChooser.showOpenDialog(stage)
    val gridData = readDataFromFile(file)
    startGame(controller, view, mainLayout, gridData)
  }

  private def readDataFromFile(file: File) : Array[String]  = {
    if (file != null) {
      val reader = new BufferedReader(new FileReader(file))
      val gridData = reader.lines().toArray.map(_.toString)
      reader.close()

      println("Loaded grid:")
      gridData.foreach(println)

      return gridData
    }
    null
  }

  private def startGame(controller: GameController, view: GameView, mainLayout: BorderPane, gridData: Array[String]): Unit = {
    controller.loadGame(gridData)
    val gridPane = view.createGrid(gridData.length, gridData(0).length)
    mainLayout.center = gridPane
  }

  private def createLevel(): Unit = {
    println("Create level functionality is not implemented yet.")
  }

  private def showResults(): Unit = {
    println("Show results functionality is not implemented yet.")
  }
}
