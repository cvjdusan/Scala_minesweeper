import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ComboBox, ListView, Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, GridPane, StackPane, VBox}
import scalafx.stage.FileChooser

import java.io.{BufferedReader, File, FileReader}
import scalafx.scene.control._

object MinesweeperApp extends JFXApp3 {

  val bestResults = scala.collection.mutable.ListBuffer[(String, Int)]()

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

    val showGameOverMessage: () => Unit = () => {
      val score = controller.endGameSuccessfully()
      score.foreach { s =>
        val playerName = "Player 1"
        updateResults(playerName, s)
      }
      new Alert(AlertType.Information) {
        title = "Game Over"
        headerText =  "💣 BOOM 💣 Game over!"
        contentText = score.map(s => s"Your score: $s").getOrElse("No score recorded.")
      }.showAndWait()
    }

    val view = new GameView(controller, showGameOverMessage)

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
                onAction = _ => createLevel(mainLayout)
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
    controller.startGameTime()
    val gridPane = view.createGrid(gridData.length, gridData(0).length)
    mainLayout.center = gridPane
  }

  private def refreshLevelView(controller: GameController, gridPane: GridPane): Unit = {
    gridPane.children.clear()
    val grid = controller.getGrid()
    for (row <- grid.indices; col <- grid(row).indices) {
      val cell = grid(row)(col)
      val button = new Label {
        prefWidth = 40
        prefHeight = 40
        alignment = Pos.Center
        style = if (cell.isMine) "-fx-background-color: red;" else "-fx-background-color: lightgray;"
        text = if (cell.isMine) "💣" else if (cell.adjacentMines > 0) cell.adjacentMines.toString else ""
      }
      gridPane.add(button, col, row)
    }
  }

  private def saveLevelToFile(controller: GameController): Unit = {
    val fileChooser = new FileChooser {
      title = "Save Level"
      extensionFilters.add(new FileChooser.ExtensionFilter("Text Files", "*.txt"))
    }
    val file = fileChooser.showSaveDialog(stage)
    if (file != null) {
      val writer = new java.io.PrintWriter(file)
      try {
        controller.getGrid().foreach { row =>
          writer.println(row.map(cell => if (cell.isMine) "#" else "-").mkString(""))
        }
      } finally {
        writer.close()
      }
      new Alert(AlertType.Information) {
        title = "Save Successful"
        headerText = "Level saved successfully!"
      }.showAndWait()
    }
  }

  private def createLevel(mainLayout: BorderPane): Unit = {
    val controller = new GameController()
    controller.initGrid(1, 1)

    val gridPane = new GridPane {
      hgap = 2
      vgap = 2
    }
    refreshLevelView(controller, gridPane)

    val options = Seq(
      "Add Row at Beginning",
      "Add Row at End",
      "Add Column at Beginning",
      "Add Column at End",
      "Remove Row at Beginning",
      "Remove Row at End",
      "Remove Column at Beginning",
      "Remove Column at End",
      "Toggle Cell Type",
      "Clear Sector"
    )

    val actionButton = new Button("Perform Action")
    val saveButton = new Button("Save Level")

    val optionsComboBox = new ComboBox[String] {
      items = ObservableBuffer(options: _*)
      promptText = "Choose an action"
    }

    val actionsBox = new VBox {
      spacing = 10
      alignment = Pos.TopCenter
      children = Seq(optionsComboBox, actionButton, saveButton)
    }

    actionButton.onAction = _ => {
      optionsComboBox.value.value match {
        case "Add Row at Beginning"    => controller.addRowBegin()
        case "Add Row at End"          => controller.addRowEnd()
        case "Add Column at Beginning" => controller.addColumnBegin()
        case "Add Column at End"       => controller.addColumnEnd()
        case "Remove Row at Beginning" => controller.removeRowBegin()
        case "Remove Row at End"       => controller.removeRowEnd()
        case "Remove Column at Beginning" => controller.removeColumnBegin()
        case "Remove Column at End"       => controller.removeColumnEnd()
        case "Toggle Cell Type" =>
          val row = promptForInt("Enter Row:")
          val col = promptForInt("Enter Column:")
          controller.toggleCellType(row, col)
        case "Clear Sector" =>
          val topLeftRow = promptForInt("Enter Top-Left Row:")
          val topLeftCol = promptForInt("Enter Top-Left Column:")
          val bottomRightRow = promptForInt("Enter Bottom-Right Row:")
          val bottomRightCol = promptForInt("Enter Bottom-Right Column:")
          controller.clearSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)
        case _ => new Alert(AlertType.Warning) {
          title = "Invalid Action"
          contentText = "Please select a valid action."
        }.showAndWait()
      }
      refreshLevelView(controller, gridPane)
    }

    saveButton.onAction = _ => saveLevelToFile(controller)

    mainLayout.center = new VBox {
      spacing = 10
      alignment = Pos.TopCenter
      children = Seq(gridPane, actionsBox)
    }
  }



  private def promptForInt(message: String): Int = {
    val dialog = new TextInputDialog(defaultValue = "0") {
      title = "Input Required"
      headerText = message
    }
    dialog.showAndWait().map(_.toInt).getOrElse(0)
  }


  private def showResults(): Unit = {
    val resultsText = if (bestResults.isEmpty) {
      "No results available yet."
    } else {
      bestResults.zipWithIndex.map { case ((name, score), idx) =>
        s"${idx + 1}. $name: $score"
      }.mkString("\n")
    }

    new Alert(AlertType.Information) {
      title = "Best Results"
      headerText = "Top Scores"
      contentText = resultsText
    }.showAndWait()
  }

  def updateResults(playerName: String, score: Int): Unit = {
    bestResults += ((playerName, score))
    bestResults.sortBy(-_._2)
  }

}
