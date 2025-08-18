import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ComboBox, ListView, Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, StackPane, VBox}
import scalafx.stage.FileChooser

import java.io.{BufferedReader, File, FileReader}
import scalafx.scene.control._
import operations.{CentralSymmetry, Reflection, Rotation, Translation}

import scala.util._

object MinesweeperApp extends JFXApp3 {

  val bestResults = scala.collection.mutable.ListBuffer[(String, Long)]()
  val difficulties = Seq("Beginner", "Normal", "Advanced")
  val levels = Map(
    "Beginner" -> Seq("Level 1", "Level 2", "Random"),
    "Normal" -> Seq("Level 1", "Level 2", "Random"),
    "Advanced" -> Seq("Level 1", "Level 2", "Random")
  )

  private def showGameOverMessage(): Unit = {
    val controller = new GameController()
    val result = controller.endGame(Game.get)
    result.foreach { case (initialScore, timeSpent, clicks, finalScore) =>
      val playerName = "Player 1"
      updateResults(playerName, finalScore)

      new Alert(AlertType.Information) {
        title = "Game Over"
        headerText = "💣 BOOM 💣 Game over!"
        contentText =
          s"""
             |Initial Score: $initialScore
             |Time Spent: $timeSpent seconds
             |Clicks: $clicks
             |Final Score: $finalScore
             |""".stripMargin
      }.showAndWait()
    }
  }

  private def showGameWinMessage(): Unit = {
    val controller = new GameController()
    val result = controller.endGame(Game.get)
    result.foreach { case (initialScore, timeSpent, clicks, finalScore) =>
      val playerName = "Player 1"
      updateResults(playerName, finalScore)

      new Alert(AlertType.Information) {
        title = "Congrats!"
        headerText = "You have won!"
        contentText =
          s"""
             |Initial Score: $initialScore
             |Time Spent: $timeSpent seconds
             |Clicks: $clicks
             |Final Score: $finalScore
             |""".stripMargin
      }.showAndWait()
    }
  }

  private lazy val commit: GameCommit = {
    val controller = new GameController()
    new GameCommit(
      controller,
      (st, over) => view.updateView(controller.getGrid(st), isGameOver = over),
      showGameOverMessage,
      showGameWinMessage
    )
  }

  private lazy val view: GameView = {
    val controller = new GameController()
    new GameView(controller, showGameOverMessage, showGameWinMessage, commit)
  }
  
  override def start(): Unit = {
    val controller = new GameController()

    val difficultyComboBox = createDifficultyBox()
    val levelsListView = createLevelList()
    val startButton = createStartButton()

    val scoreLabel = createScoreLabel(controller)


    difficultyComboBox.onAction = _ => {
      val selectedDifficulty = difficultyComboBox.value.value
      levelsListView.items = ObservableBuffer[String](levels.getOrElse(selectedDifficulty, Seq.empty[String]): _*)
      levelsListView.disable = false
      startButton.disable = levelsListView.selectionModel().getSelectedItem == null
    }

    levelsListView.selectionModel().selectedItemProperty().addListener { (_, _, newValue) =>
      startButton.disable = newValue == null
    }

    val hintButton = new Button("Hint") {
      visible = false
      onAction = _ => {
        val (suggestion, _) = controller.suggestMove(Game.get)

        commit.update(
          (controller, state) => controller.suggestMove(state)._2,
          state => Game.get.isGameLost
        )

        if (suggestion.isEmpty) {
          new Alert(AlertType.Information) {
            title = "Hint"
            headerText = "No Moves Available"
            contentText = "There are no valid moves left!"
          }.showAndWait()
        }
      }
    }

    lazy val mainLayout: BorderPane = new BorderPane {
      top = new VBox {
        spacing = 10
        alignment = Pos.Center
        children = Seq(
          new MenuBar {
            menus = Seq(
              new Menu("Options") {
                items = Seq(
                  new MenuItem("New game") {
                    onAction = _ => resetToSelection(difficultyComboBox, levelsListView, startButton, mainLayout, scoreLabel, hintButton, controller)
                  },
                  new MenuItem("Save game") {
                    onAction = _ => saveGame(controller)
                  },
                  new MenuItem("Play moves") {
                    onAction = _ => playMoves(controller)
                  },
                  new MenuItem("Load game") {
                    onAction = _ => loadGame(controller, mainLayout, scoreLabel, hintButton)
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
          },
          new HBox {
            spacing = 10
            alignment = Pos.Center
            children = Seq(scoreLabel, hintButton)
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

    startButton.onAction = _ => startNewGame(controller, view, difficultyComboBox, levelsListView, mainLayout, scoreLabel, hintButton)

    stage = new JFXApp3.PrimaryStage {
      title = "Minesweeper"
      width = 800
      height = 600
      scene = new Scene {
        root = mainLayout
      }
    }
  }

  private def createDifficultyBox(): ComboBox[String] =
    new ComboBox[String] {
      promptText = "Choose difficulty"
      items = ObservableBuffer[String](difficulties: _*)
    }

  private def createLevelList(): ListView[String] =
    new ListView[String] {
      prefHeight = 100
      disable = true
    }

  private def createStartButton(): Button =
    new Button("Start Game") {
      disable = true
    }

  private def createScoreLabel(controller: GameController): Label =
    new Label("Score: 1000") {
      visible = false
    }

  def updateScoreLabel(controller: GameController, scoreLabel: Label, view: GameView): Unit = {
    scoreLabel.text = s"Score: ${controller.getScore(Game.get)}"
  }

  private def startNewGame(controller: GameController,
                           view: GameView,
                           difficultyComboBox: ComboBox[String],
                           levelsListView: ListView[String],
                           mainLayout: BorderPane,
                           scoreLabel: Label,
                           hintButton: Button): Unit = {
    val filesPath = "C:\\Users\\cvdus\\OneDrive\\Documents\\Minesweeper_levels\\"
    val fileExtension = ".txt"
    val selectedDifficulty = difficultyComboBox.value.value
    var selectedLevel = levelsListView.selectionModel().getSelectedItem

    if (selectedDifficulty == null || selectedLevel == null) {
      new Alert(AlertType.Warning) {
        title = "Warning"
        headerText = "Invalid Selection"
        contentText = "Please select both a difficulty and a level!"
      }.showAndWait()
      return
    }

    val fileName = createFileName(selectedDifficulty, selectedLevel)
    val filePath = filesPath + fileName + fileExtension

    readDataFromFile(getFile(filePath).get) match {
      case Right(gridData) =>
        startGame(controller, view, mainLayout, gridData)
      case Left(error) =>
        new Alert(AlertType.Error) {
          title = "Load Error"
          headerText = "Failed to read level file"
          contentText = s"Error: $error"
        }.showAndWait()
    }

    scoreLabel.visible = true
    hintButton.visible = true
  }

  private def createFileName(difficulty: String, selectedLevel: String): String = {
    if (selectedLevel.equals("Random"))
      difficulty.toLowerCase.concat("_").concat("level_".concat(Random.between(1, 3).toString).toLowerCase)
    else
      difficulty.toLowerCase.concat("_").concat(selectedLevel.replace(" ", "_").toLowerCase)
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

  private def resetToSelection(difficultyComboBox: ComboBox[String],
                               levelsListView: ListView[String],
                               startButton: Button,
                               mainLayout: BorderPane,
                               scoreLabel: Label,
                               hintButton: Button,
                               controller: GameController): Unit = {
    difficultyComboBox.value = null
    levelsListView.items.value.clear()
    levelsListView.disable = true
    startButton.disable = true
    scoreLabel.visible = false
    hintButton.visible = false

    resetScoreView(controller, scoreLabel, view)

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

  private def resetScoreView(controller: GameController, scoreLabel: Label, view: GameView) = {
    commit.update((controller, state) => controller.initScore(state))
    updateScoreLabel(controller, scoreLabel, view)
  }

  private def loadGame(controller: GameController, mainLayout: BorderPane, scoreLabel: Label, hintButton: Button): Unit = {
    val fileChooser = new FileChooser {
      title = "Load Game"
      extensionFilters.add(new FileChooser.ExtensionFilter("Game Files", "*.txt"))
    }
    Option(fileChooser.showOpenDialog(stage)).foreach { file =>
      readDataFromFile(file) match {
        case Right(gridData) =>
          scoreLabel.visible = true
          hintButton.visible = true
          startGame(controller, view, mainLayout, gridData)
        case Left(error) =>
          new Alert(AlertType.Error) {
            title = "Load Error"
            headerText = "Failed to load game"
            contentText = s"Error: $error"
          }.showAndWait()
      }
    }
  }

  def readDataFromFile(file: File): Either[String, Array[String]] = {
    if (file == null) {
      Left("No file selected")
    } else {
      Try {
        val reader = new BufferedReader(new FileReader(file))
        val gridData = reader.lines().toArray.map(_.toString)
        reader.close()
        gridData
      }.toEither.left.map(_.getMessage)
    }
  }

  private def startGame(controller: GameController, view: GameView, mainLayout: BorderPane, gridData: Array[String]): Unit = {
    Try {
      val newState = controller.loadGame(GameState.empty, gridData) match {
        case Right(state) => state
        case Left(error) => throw new RuntimeException(error)
      }

      val gridPane = view.createGrid(gridData.length, gridData(0).length)
      commit.update((c, s) => controller.startGameTime(newState))
      mainLayout.center = gridPane
    } match {
      case Success(_) => // Game started successfully
      case Failure(exception) =>
        new Alert(AlertType.Error) {
          title = "Game Start Error"
          headerText = "Failed to start game"
          contentText = s"Error: ${exception.getMessage}"
        }.showAndWait()
    }
  }

  private def refreshLevelView(controller: GameController, gridPane: GridPane, view: GameView): Unit = {
    gridPane.children.clear()
    val grid = controller.getGrid(Game.get)
    for (row <- grid.indices; col <- grid(row).indices) {
      val cell = grid(row)(col)
      val button = new Label {
        prefWidth = 40
        prefHeight = 40
        alignment = Pos.Center
        style = if (cell.isMine) "-fx-background-color: red;" else "-fx-background-color: lightgray;"
        text = if (cell.isMine) "💣" else ""
      }
      gridPane.add(button, col, row)
    }
  }

  private def saveLevelToFile(controller: GameController, view: GameView): Unit = {
    val fileChooser = new FileChooser {
      title = "Save Level"
      extensionFilters.add(new FileChooser.ExtensionFilter("Text Files", "*.txt"))
    }
    val file = fileChooser.showSaveDialog(stage)
    if (file != null) {
      val writer = new java.io.PrintWriter(file)
      try {
        controller.getGrid(Game.get).foreach { row =>
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
    var levelState = controller.initGrid(5, 5)

    val gridPane = new GridPane {
      hgap = 2
      vgap = 2
    }

    def refreshLevelView(): Unit = {
      gridPane.children.clear()
      val grid = controller.getGrid(levelState)
      for (row <- grid.indices; col <- grid(row).indices) {
        val cell = grid(row)(col)
        val button = new Label {
          prefWidth = 40
          prefHeight = 40
          alignment = Pos.Center
          style = if (cell.isMine) "-fx-background-color: red;" else "-fx-background-color: lightgray;"
          text = if (cell.isMine) "💣" else ""
        }
        gridPane.add(button, col, row)
      }
    }

    refreshLevelView()

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

    actionButton.onAction = _ => {
      optionsComboBox.value.value match {
        case "Add Row at Beginning" =>
          levelState = controller.addRowBegin(levelState)
          refreshLevelView()
        case "Add Row at End" =>
          levelState = controller.addRowEnd(levelState)
          refreshLevelView()
        case "Add Column at Beginning" =>
          levelState = controller.addColumnBegin(levelState)
          refreshLevelView()
        case "Add Column at End" =>
          levelState = controller.addColumnEnd(levelState)
          refreshLevelView()
        case "Remove Row at Beginning" =>
          levelState = controller.removeRowBegin(levelState)
          refreshLevelView()
        case "Remove Row at End" =>
          levelState = controller.removeRowEnd(levelState)
          refreshLevelView()
        case "Remove Column at Beginning" =>
          levelState = controller.removeColumnBegin(levelState)
          refreshLevelView()
        case "Remove Column at End" =>
          levelState = controller.removeColumnEnd(levelState)
          refreshLevelView()
        case "Toggle Cell Type" =>
          val row = promptForInt("Enter Row:")
          val col = promptForInt("Enter Column:")
          levelState = controller.toggleMine(levelState, row, col)
          refreshLevelView()
        case "Clear Sector" =>
          val topLeftRow = promptForInt("Enter Top-Left Row:")
          val topLeftCol = promptForInt("Enter Top-Left Column:")
          val bottomRightRow = promptForInt("Enter Bottom-Right Row:")
          val bottomRightCol = promptForInt("Enter Bottom-Right Column:")
          levelState = controller.clearSector(levelState, topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)
          refreshLevelView()
        case _ => new Alert(AlertType.Warning) {
          title = "Invalid Action"
          contentText = "Please select a valid action."
        }.showAndWait()
      }
    }

    saveButton.onAction = _ => {
      val fileChooser = new FileChooser {
        title = "Save Level"
        extensionFilters.add(new FileChooser.ExtensionFilter("Text Files", "*.txt"))
      }
      val file = fileChooser.showSaveDialog(stage)
      if (file != null) {
        val writer = new java.io.PrintWriter(file)
        try {
          controller.getGrid(levelState).foreach { row =>
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

    val isometryOptions = Seq(
      "Rotate Clockwise", "Rotate Counterclockwise",
      "Reflect Horizontally", "Reflect Vertically",
      "Reflect Diagonal (Main)", "Reflect Diagonal (Secondary)",
      "Central Symmetry", "Translation"
    )

    val isometryComboBox = new ComboBox[String] {
      items = ObservableBuffer(isometryOptions: _*)
      promptText = "Choose an isometry"
    }

    val applyIsometryButton = new Button("Apply Isometry") {
      onAction = _ => {
        isometryComboBox.value.value match {
          case "Rotate Clockwise" =>
            levelState = controller.applyIsometry(levelState, Rotation(clockwise = true))
            refreshLevelView()
          case "Rotate Counterclockwise" =>
            levelState = controller.applyIsometry(levelState, Rotation(clockwise = false))
            refreshLevelView()
          case "Reflect Horizontally" =>
            levelState = controller.applyIsometry(levelState, Reflection("horizontal"))
            refreshLevelView()
          case "Reflect Vertically" =>
            levelState = controller.applyIsometry(levelState, Reflection("vertical"))
            refreshLevelView()
          case "Reflect Diagonal (Main)" =>
            levelState = controller.applyIsometry(levelState, Reflection("diagonal-main"))
            refreshLevelView()
          case "Reflect Diagonal (Secondary)" =>
            levelState = controller.applyIsometry(levelState, Reflection("diagonal-secondary"))
            refreshLevelView()
          case "Central Symmetry" =>
            levelState = controller.applyIsometry(levelState, CentralSymmetry)
            refreshLevelView()
          case "Translation" =>
            val dx = promptForInt("Enter Translation X:")
            val dy = promptForInt("Enter Translation Y:")
            levelState = controller.applyIsometry(levelState, Translation(dx, dy))
            refreshLevelView()
          case _ =>
            new Alert(AlertType.Warning) {
              title = "Invalid Isometry"
              contentText = "Please select a valid isometry."
            }.showAndWait()
        }
      }
    }

    mainLayout.center = new VBox {
      spacing = 10
      alignment = Pos.TopCenter
      children = Seq(gridPane, optionsComboBox, actionButton, isometryComboBox, applyIsometryButton, saveButton)
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

  def updateResults(playerName: String, score: Long): Unit = {
    bestResults += ((playerName, score))
    bestResults.sortBy(-_._2)
  }

  private def saveGame(controller: GameController): Unit = {
    val chooser = new FileChooser {
      title = "Save Game"
      extensionFilters.add(new FileChooser.ExtensionFilter("Text Files", "*.txt"))
    }
    Option(chooser.showSaveDialog(stage)).foreach { file =>
      controller.saveGame(Game.get, file.toPath) match {
        case Right(_) =>
          new Alert(AlertType.Information) {
            title = "Saved"
            headerText = "Game saved successfully."
          }.showAndWait()
        case Left(error) =>
          new Alert(AlertType.Error) {
            title = "Save Error"
            headerText = "Failed to save game"
            contentText = s"Error: $error"
          }.showAndWait()
      }
    }
  }

  private def playMoves(controller: GameController): Unit = {
    val chooser = new FileChooser {
      title = "Open Move Sequence"
      extensionFilters.add(new FileChooser.ExtensionFilter("Text Files", "*.txt"))
    }
    Option(chooser.showOpenDialog(stage)).foreach { file =>
      Try {
        val moves = scala.io.Source.fromFile(file).getLines().toSeq
        println(moves)

        commit.update(
          (c, s) => c.playMoves(s, moves),
          st => Game.get.isGameLost
        )

      } match {
        case Success(_) =>
          new Alert(AlertType.Information) {
            title = "Moves Played"
            headerText = "Move sequence executed successfully."
          }.showAndWait()
        case Failure(exception) =>
          new Alert(AlertType.Error) {
            title = "Play Moves Error"
            headerText = "Failed to execute moves"
            contentText = s"Error: ${exception.getMessage}"
          }.showAndWait()
      }
    }
  }
}
