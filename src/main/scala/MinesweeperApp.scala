import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, ButtonType, CheckBox, ComboBox, Label, ListView, Menu, MenuBar, MenuItem, TextField, TextInputDialog}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, StackPane, VBox}
import scalafx.stage.FileChooser

import java.io.{BufferedReader, File, FileReader}
import operations.{CentralSymmetry, ComposedIsometry, ExpandingIsometry, Isometry, IsometryComposer, NamedIsometryRegistry, Reflection, Rotation, Translation, TransparentIsometry}
import scalafx.Includes.jfxObjectProperty2sfx

import scala.util._
import scala.util.Random
import scala.util.Try
import scala.util.Success
import scala.util.Failure


object MinesweeperApp extends JFXApp3 {

  val bestResults = scala.collection.mutable.ListBuffer[(String, Long)]()
  val difficulties = Seq("Beginner", "Normal", "Advanced")
  val levels = Map(
    "Beginner" -> Seq("Level 1", "Level 2", "Random"),
    "Normal" -> Seq("Level 1", "Level 2", "Random"),
    "Advanced" -> Seq("Level 1", "Level 2", "Random")
  )

  var isometryOptions = Seq(
    "Rotate Clockwise", "Rotate Counterclockwise",
    "Reflect Horizontally", "Reflect Vertically",
    "Reflect Diagonal (Main)", "Reflect Diagonal (Secondary)",
    "Central Symmetry", "Translation" //"Compose Isometries"
  )

  private def showGameOverMessage(): Unit = {
    val controller = new GameController()
    val result = controller.endGameGameOver(Game.get)
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

        updateScoreLabel(controller, scoreLabel, view)

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
                    onAction = _ => createLevelEditor(controller, mainLayout, scoreLabel, hintButton)
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
    commit.updateSilently((_, _) => GameState.empty)
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
    commit.updateSilently((controller, state) => controller.initScore(state))
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

  private def composeIsometries(expandingCheckbox: CheckBox, transparentCheckbox: CheckBox): Option[operations.Isometry] = {
    val dialog = new Alert(AlertType.Confirmation) {
      title = "Compose Isometries"
      headerText = "Select isometries to compose"
      contentText = "Choose which isometries you want to compose:"
    }

    val rotationCheckbox = new CheckBox("Rotation")
    val reflectionCheckbox = new CheckBox("Reflection")
    val centralCheckbox = new CheckBox("Central Symmetry")
    val translationCheckbox = new CheckBox("Translation")
    val savedIsometriesCheckbox = new CheckBox("Use Saved Isometries")

    val rotationOptions = new ComboBox[String] {
      items = ObservableBuffer("Clockwise", "Counterclockwise")
      value = "Clockwise"
      disable = true
    }
    val rotationCountField = new TextField {
      text = "1"
      promptText = "Count"
      prefWidth = 50
      disable = true
    }
    rotationCheckbox.onAction = _ => {
      val enabled = rotationCheckbox.isSelected
      rotationOptions.disable = !enabled
      rotationCountField.disable = !enabled
    }

    val reflectionOptions = new ComboBox[String] {
      items = ObservableBuffer("Horizontal", "Vertical", "Diagonal (Main)", "Diagonal (Secondary)")
      value = "Horizontal"
      disable = true
    }
    val reflectionCountField = new TextField {
      text = "1"
      promptText = "Count"
      prefWidth = 50
      disable = true
    }
    val reflectionPositionField = new TextField {
      text = "0"
      promptText = "Axis Position"
      prefWidth = 80
      disable = true
    }
    reflectionCheckbox.onAction = _ => {
      val enabled = reflectionCheckbox.isSelected
      reflectionOptions.disable = !enabled
      reflectionCountField.disable = !enabled
      reflectionPositionField.disable = !enabled
    }

    val translationXField = new TextField {
      text = "1"
      promptText = "X offset"
      prefWidth = 60
      disable = true
    }
    val translationYField = new TextField {
      text = "1"
      promptText = "Y offset"
      prefWidth = 60
      disable = true
    }
    val translationCountField = new TextField {
      text = "1"
      promptText = "Count"
      prefWidth = 50
      disable = true
    }
    translationCheckbox.onAction = _ => {
      val enabled = translationCheckbox.isSelected
      translationXField.disable = !enabled
      translationYField.disable = !enabled
      translationCountField.disable = !enabled
    }

    val savedIsometriesComboBox = new ComboBox[String] {
      items = ObservableBuffer(NamedIsometryRegistry.getAllNames: _*)
      promptText = "Select saved isometry"
      disable = true
    }
    val savedIsometriesCountField = new TextField {
      text = "1"
      promptText = "Count"
      prefWidth = 50
      disable = true
    }
    
    savedIsometriesCheckbox.onAction = _ => {
      val enabled = savedIsometriesCheckbox.isSelected
      savedIsometriesComboBox.disable = !enabled
      savedIsometriesCountField.disable = !enabled
    }

    val rotationLayout = new HBox {
      spacing = 10
      children = Seq(rotationCheckbox, rotationOptions, new Label("Count:"), rotationCountField)
    }

    val reflectionLayout = new HBox {
      spacing = 10
      children = Seq(reflectionCheckbox, reflectionOptions, new Label("Position:"), reflectionPositionField, new Label("Count:"), reflectionCountField)
    }

    val translationLayout = new HBox {
      spacing = 10
      children = Seq(
        translationCheckbox,
        new Label("X:"),
        translationXField,
        new Label("Y:"),
        translationYField,
        new Label("Count:"),
        translationCountField
      )
    }

    val savedIsometriesLayout = new HBox {
      spacing = 10
      children = Seq(
        savedIsometriesCheckbox,
        savedIsometriesComboBox,
        new Label("Count:"),
        savedIsometriesCountField
      )
    }

    val optionsLayout = new VBox {
      spacing = 10
      children = Seq(
        rotationLayout,
        reflectionLayout,
        centralCheckbox,
        translationLayout,
        savedIsometriesLayout
      )
    }

    dialog.dialogPane().setContent(optionsLayout)

    val result = dialog.showAndWait()
    result.flatMap { _ =>
      val expanding = expandingCheckbox.isSelected
      val transparent = transparentCheckbox.isSelected

      val isometries = Seq.newBuilder[Isometry]

      if (rotationCheckbox.isSelected) {
        val clockwise = rotationOptions.value.value == "Clockwise"
        val count = Try(rotationCountField.text.value.toInt).getOrElse(1)
        val rotation = if (transparent) {
          if (expanding) {
            new Rotation(clockwise) with ExpandingIsometry with TransparentIsometry
          } else {
            new Rotation(clockwise) with TransparentIsometry
          }
        } else {
          if (expanding) {
            new Rotation(clockwise) with ExpandingIsometry
          } else {
            new Rotation(clockwise)
          }
        }
        for (_ <- 1 to count) {
          isometries += rotation
        }
      }


      if (reflectionCheckbox.isSelected) {
        val axis = reflectionOptions.value.value match {
          case "Horizontal" => "horizontal"
          case "Vertical" => "vertical"
          case "Diagonal (Main)" => "diagonal-main"
          case "Diagonal (Secondary)" => "diagonal-secondary"
          case _ => "horizontal"
        }
        val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
        val reflection = if (transparent) {
          if (expanding) {
            new Reflection(axis, Some(position)) with ExpandingIsometry with TransparentIsometry
          } else {
            new Reflection(axis, Some(position)) with TransparentIsometry
          }
        } else {
          if (expanding) {
            new Reflection(axis, Some(position)) with ExpandingIsometry
          } else {
            new Reflection(axis, Some(position))
          }
        }
        val count = Try(reflectionCountField.text.value.toInt).getOrElse(1)

        for (_ <- 1 to count) {
          isometries += reflection
        }
      }

      if (centralCheckbox.isSelected) {
        val symmetry = if (transparent) {
          if (expanding) {
            new CentralSymmetry() with ExpandingIsometry with TransparentIsometry
          } else {
            new CentralSymmetry() with TransparentIsometry
          }
        } else {
          if (expanding) {
            new CentralSymmetry() with ExpandingIsometry
          } else {
            new CentralSymmetry()
          }
        }

        isometries += symmetry
      }

      if (translationCheckbox.isSelected) {
        val dx = Try(translationXField.text.value.toInt).getOrElse(1)
        val dy = Try(translationYField.text.value.toInt).getOrElse(1)
        val count = Try(translationCountField.text.value.toInt).getOrElse(1)

        val finalDx = dx * count
        val finalDy = dy * count

        val translation = if (transparent) {
          if (expanding) {
            new Translation(finalDx, finalDy) with ExpandingIsometry with TransparentIsometry
          } else {
            new Translation(finalDx, finalDy) with TransparentIsometry
          }
        } else {
          if (expanding) {
            new Translation(finalDx, finalDy) with ExpandingIsometry
          } else {
            new Translation(finalDx, finalDy)
          }
        }
        isometries += translation
      }

      if (savedIsometriesCheckbox.isSelected) {
        val savedIsometryName = savedIsometriesComboBox.value.value
        if (savedIsometryName != null) {
          val savedIsometry = NamedIsometryRegistry.getIsometry(savedIsometryName)
          savedIsometry match {
            case Some(iso) =>
              val count = Try(savedIsometriesCountField.text.value.toInt).getOrElse(1)
              for (_ <- 1 to count) {
                isometries += iso
              }
            case None =>

              new Alert(AlertType.Error) {
                title = "Error"
                headerText = "Saved isometry not found"
                contentText = s"Could not find saved isometry: $savedIsometryName"
              }.showAndWait()
          }
        }
      }

      val result = isometries.result()
      if (result.nonEmpty) {
        try {
          val composedIsometry = IsometryComposer.compose(result: _*)

          Some(composedIsometry)
        } catch {
          case e: IllegalArgumentException =>
            new Alert(AlertType.Error) {
              title = "Composition Error"
              headerText = "Invalid isometry parameters"
              contentText = s"""
                |Failed to create composition due to invalid parameters:
                |
                |Error: ${e.getMessage}
                |
              """.stripMargin
            }.showAndWait()
            None
            
          case e: Exception =>
            new Alert(AlertType.Error) {
              title = "Composition Error"
              headerText = "Unexpected error occurred"
              contentText = s"""
                |Failed to create composition due to unexpected error:
                |
                |Error: ${e.getMessage}
                |Please try again or check the console for more details.
              """.stripMargin
            }.showAndWait()
            None
        }
      } else {
        new Alert(AlertType.Warning) {
          title = "No Isometries Selected"
          headerText = "Cannot create composition"
          contentText = "Please select at least one isometry to compose."
        }.showAndWait()

        None
      }
    }
  }

  private def createLevelEditor(controller: GameController, mainLayout: BorderPane, scoreLabel: Label, hintButton: Button): Unit = {
    var levelState = controller.initGrid(5, 5)
    val originalLevelState = levelState
    val gridPane = new GridPane {
      hgap = 2
      vgap = 2
    }
    hintButton.visible = false
    scoreLabel.visible = false

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
      "Clear Sector",
      "Validate Level"
    )

    val actionButton = new Button("Perform Action")
    val saveButton = new Button("Save Level")
    val resetButton = new Button("Reset to Original")

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
        case "Validate Level" =>
          val difficulty = promptForDifficulty()
          val validationResult = controller.validateLevel(levelState, difficulty)
          validationResult match {
            case Right(_) =>
              new Alert(AlertType.Information) {
                title = "Validation Successful"
                headerText = "Level is valid"
                contentText = s"Level meets $difficulty requirements"
              }.showAndWait()
            case Left(error) =>
              new Alert(AlertType.Warning) {
                title = "Validation Failed"
                headerText = "Level does not meet requirements"
                contentText = s"Error: $error"
              }.showAndWait()
          }
        case _ => new Alert(AlertType.Warning) {
          title = "Invalid Action"
          contentText = "Please select a valid action."
        }.showAndWait()
      }
    }

    saveButton.onAction = _ => {
      val validationResult = controller.validateLevel(levelState, "Normal")
      validationResult match {
        case Right(_) =>
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
        case Left(error) =>
          new Alert(AlertType.Error) {
            title = "Validation Error"
            headerText = "Level validation failed"
            contentText = s"Cannot save level: $error"
          }.showAndWait()
      }
    }


    val isometryComboBox = new ComboBox[String] {
      items = ObservableBuffer(isometryOptions: _*)
      promptText = "Choose an isometry"
    }


    val useSectorCheckbox = new CheckBox("Apply to sector instead of whole grid")
    val sectorLabel = new Label("Sector (optional):")
    val topLeftRowField = new TextField {
      text = "0"
    }
    val topLeftColField = new TextField {
      text = "0"
    }
    val bottomRightRowField = new TextField {
      text = "2"
    }
    val bottomRightColField = new TextField {
      text = "2"
    }

    val pivotLabel = new Label("Pivot:")
    val pivotRowField = new TextField {
      text = "1"
    }
    val pivotColField = new TextField {
      text = "1"
    }

    val sectorGrid = new GridPane {
      hgap = 5
      vgap = 5
      add(new Label("Top-Left:"), 0, 0)
      add(topLeftRowField, 1, 0)
      add(new Label(","), 2, 0)
      add(topLeftColField, 3, 0)
      add(new Label("Bottom-Right:"), 0, 1)
      add(bottomRightRowField, 1, 1)
      add(new Label(","), 2, 1)
      add(bottomRightColField, 3, 1)
    }

    val pivotGrid = new GridPane {
      hgap = 5
      vgap = 5
      add(new Label("Pivot:"), 0, 0)
      add(pivotRowField, 1, 0)
      add(new Label(","), 2, 0)
      add(pivotColField, 3, 0)
    }

    val expandingCheckbox = new CheckBox("Expanding")
    val transparentCheckbox = new CheckBox("Transparent")
    val reflectionPositionField = new TextField {
      text = "0"
      promptText = "Axis Position"
      prefWidth = 80
    }

    val optionsGrid = new GridPane {
      hgap = 5
      vgap = 5
      add(expandingCheckbox, 0, 0)
      add(transparentCheckbox, 1, 0)
      add(new Label("Reflection Position:"), 0, 1)
      add(reflectionPositionField, 1, 1)
    }

    def updateIsometryOptions(): Unit = {
      val savedNames = NamedIsometryRegistry.getAllNames
      val customOptions = savedNames.filter(name => !MinesweeperApp.isometryOptions.contains(name))
      if (customOptions.nonEmpty) {
        MinesweeperApp.isometryOptions = MinesweeperApp.isometryOptions ++ customOptions

        try {
          isometryComboBox.items = ObservableBuffer(MinesweeperApp.isometryOptions: _*)
        } catch {
          case _: Throwable =>
        }
      }
    }

    val applyIsometryButton: Button = new Button("Apply Isometry") {
      onAction = _ => {
        if (useSectorCheckbox.isSelected) {
          try {
            val topLeftRow = topLeftRowField.text.value.toInt
            val topLeftCol = topLeftColField.text.value.toInt
            val bottomRightRow = bottomRightRowField.text.value.toInt
            val bottomRightCol = bottomRightColField.text.value.toInt
            val pivotRow = pivotRowField.text.value.toInt
            val pivotCol = pivotColField.text.value.toInt

            val sector = controller.createSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)
            val pivot = (pivotRow, pivotCol)

            if (sector.isValid) {
              isometryComboBox.value.value match {
                case "Rotate Clockwise" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val rotation = if (transparent) {
                    if (expanding) {
                      new Rotation(clockwise = true) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Rotation(clockwise = true) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Rotation(clockwise = true) with ExpandingIsometry
                    } else {
                      new Rotation(clockwise = true)
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, rotation, sector, pivot)
                  refreshLevelView()
                case "Rotate Counterclockwise" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val rotation = if (transparent) {
                    if (expanding) {
                      new Rotation(clockwise = false) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Rotation(clockwise = false) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Rotation(clockwise = false) with ExpandingIsometry
                    } else {
                      new Rotation(clockwise = false)
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, rotation, sector, pivot)
                  refreshLevelView()
                case "Reflect Horizontally" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
                  val reflection = if (transparent) {
                    if (expanding) {
                      new Reflection("horizontal", Some(position)) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Reflection("horizontal", Some(position)) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Reflection("horizontal", Some(position)) with ExpandingIsometry
                    } else {
                      new Reflection("horizontal", Some(position))
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, reflection, sector, pivot)
                  refreshLevelView()
                case "Reflect Vertically" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
                  val reflection = if (transparent) {
                    if (expanding) {
                      new Reflection("vertical", Some(position)) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Reflection("vertical", Some(position)) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Reflection("vertical", Some(position)) with ExpandingIsometry
                    } else {
                      new Reflection("vertical", Some(position))
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, reflection, sector, pivot)
                  refreshLevelView()
                case "Reflect Diagonal (Main)" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
                  val reflection = if (transparent) {
                    if (expanding) {
                      new Reflection("diagonal-main", Some(position)) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Reflection("diagonal-main", Some(position)) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Reflection("diagonal-main", Some(position)) with ExpandingIsometry
                    } else {
                      new Reflection("diagonal-main", Some(position))
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, reflection, sector, pivot)
                  refreshLevelView()
                case "Reflect Diagonal (Secondary)" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
                  val reflection = if (transparent) {
                    if (expanding) {
                      new Reflection("diagonal-secondary", Some(position)) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Reflection("diagonal-secondary", Some(position)) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Reflection("diagonal-secondary", Some(position)) with ExpandingIsometry
                    } else {
                      new Reflection("diagonal-secondary", Some(position))
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, reflection, sector, pivot)
                  refreshLevelView()
                case "Central Symmetry" =>
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val symmetry = if (transparent) {
                    if (expanding) {
                      new CentralSymmetry() with ExpandingIsometry with TransparentIsometry
                    } else {
                      new CentralSymmetry() with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new CentralSymmetry() with ExpandingIsometry
                    } else {
                      new CentralSymmetry()
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, symmetry, sector, pivot)
                  refreshLevelView()
                case "Translation" =>
                  val dx = promptForInt("Enter Translation X:")
                  val dy = promptForInt("Enter Translation Y:")
                  val expanding = expandingCheckbox.isSelected
                  val transparent = transparentCheckbox.isSelected
                  val translation = if (transparent) {
                    if (expanding) {
                      new Translation(dx, dy) with ExpandingIsometry with TransparentIsometry
                    } else {
                      new Translation(dx, dy) with TransparentIsometry
                    }
                  } else {
                    if (expanding) {
                      new Translation(dx, dy) with ExpandingIsometry
                    } else {
                      new Translation(dx, dy)
                    }
                  }
                  levelState = controller.applyIsometryToSector(levelState, translation, sector, pivot)
                  refreshLevelView()
                case "Compose Isometries" =>

                  val composedIsometry = composeIsometries(expandingCheckbox, transparentCheckbox)
                  composedIsometry.foreach { newIso =>
                    levelState = controller.applyIsometryToSector(levelState, newIso, sector, pivot)
                    refreshLevelView()
                  }
                case _ =>

                  val savedIsometry = NamedIsometryRegistry.getIsometry(isometryComboBox.value.value)
                  savedIsometry match {
                    case Some(iso) =>
                      levelState = controller.applyIsometryToSector(levelState, iso, sector, pivot)
                      refreshLevelView()
                    case None =>
                      new Alert(AlertType.Warning) {
                        title = "Invalid Isometry"
                        contentText = "Please select a valid isometry."
                      }.showAndWait()
                  }
              }
            } else {
              new Alert(AlertType.Warning) {
                title = "Invalid Sector"
                contentText = "Sector coordinates are invalid. Please check your input."
              }.showAndWait()
            }
          } catch {
            case _: NumberFormatException =>
              new Alert(AlertType.Warning) {
                title = "Invalid Input"
                contentText = "Please enter valid numbers for sector and pivot coordinates."
              }.showAndWait()
          }
        } else {
          isometryComboBox.value.value match {
            case "Rotate Clockwise" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val rotation = if (transparent) {
                if (expanding) {
                  new Rotation(clockwise = true) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Rotation(clockwise = true) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Rotation(clockwise = true) with ExpandingIsometry
                } else {
                  new Rotation(clockwise = true)
                }
              }
              levelState = controller.applyIsometry(levelState, rotation)
              refreshLevelView()
            case "Rotate Counterclockwise" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val rotation = if (transparent) {
                if (expanding) {
                  new Rotation(clockwise = false) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Rotation(clockwise = false) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Rotation(clockwise = false) with ExpandingIsometry
                } else {
                  new Rotation(clockwise = false)
                }
              }
              levelState = controller.applyIsometry(levelState, rotation)
              refreshLevelView()
            case "Reflect Horizontally" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
              val reflection = if (transparent) {
                if (expanding) {
                  new Reflection("horizontal", Some(position)) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Reflection("horizontal", Some(position)) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Reflection("horizontal", Some(position)) with ExpandingIsometry
                } else {
                  new Reflection("horizontal", Some(position))
                }
              }
              levelState = controller.applyIsometry(levelState, reflection)
              refreshLevelView()
            case "Reflect Vertically" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
              val reflection = if (transparent) {
                if (expanding) {
                  new Reflection("vertical", Some(position)) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Reflection("vertical", Some(position)) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Reflection("vertical", Some(position)) with ExpandingIsometry
                } else {
                  new Reflection("vertical", Some(position))
                }
              }
              levelState = controller.applyIsometry(levelState, reflection)
              refreshLevelView()
            case "Reflect Diagonal (Main)" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
              val reflection = if (transparent) {
                if (expanding) {
                  new Reflection("diagonal-main", Some(position)) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Reflection("diagonal-main", Some(position)) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Reflection("diagonal-main", Some(position)) with ExpandingIsometry
                } else {
                  new Reflection("diagonal-main", Some(position))
                }
              }
              levelState = controller.applyIsometry(levelState, reflection)
              refreshLevelView()
            case "Reflect Diagonal (Secondary)" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val position = Try(reflectionPositionField.text.value.toInt).getOrElse(0)
              val reflection = if (transparent) {
                if (expanding) {
                  new Reflection("diagonal-secondary", Some(position)) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Reflection("diagonal-secondary", Some(position)) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Reflection("diagonal-secondary", Some(position)) with ExpandingIsometry
                } else {
                  new Reflection("diagonal-secondary", Some(position))
                }
              }
              levelState = controller.applyIsometry(levelState, reflection)
              refreshLevelView()
            case "Central Symmetry" =>
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val symmetry = if (transparent) {
                if (expanding) {
                  new CentralSymmetry() with ExpandingIsometry with TransparentIsometry
                } else {
                  new CentralSymmetry() with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new CentralSymmetry() with ExpandingIsometry
                } else {
                  new CentralSymmetry()
                }
              }
              levelState = controller.applyIsometry(levelState, symmetry)
              refreshLevelView()
            case "Translation" =>
              val dx = promptForInt("Enter Translation X:")
              val dy = promptForInt("Enter Translation Y:")
              val expanding = expandingCheckbox.isSelected
              val transparent = transparentCheckbox.isSelected
              val translation = if (transparent) {
                if (expanding) {
                  new Translation(dx, dy) with ExpandingIsometry with TransparentIsometry
                } else {
                  new Translation(dx, dy) with TransparentIsometry
                }
              } else {
                if (expanding) {
                  new Translation(dx, dy) with ExpandingIsometry
                } else {
                  new Translation(dx, dy)
                }
              }
              levelState = controller.applyIsometry(levelState, translation)
              refreshLevelView()
            case "Compose Isometries" =>

              val composedIsometry = composeIsometries(expandingCheckbox, transparentCheckbox)
              composedIsometry.foreach { newIso =>
                levelState = controller.applyIsometry(levelState, newIso)
                refreshLevelView()
              }
            case _ =>

              val savedIsometry = NamedIsometryRegistry.getIsometry(isometryComboBox.value.value)
              savedIsometry match {
                case Some(iso) =>
                  levelState = controller.applyIsometry(levelState, iso)
                  refreshLevelView()
                case None =>
                  new Alert(AlertType.Warning) {
                    title = "Invalid Isometry"
                    contentText = "Please select a valid isometry."
                  }.showAndWait()
              }
          }
        }
      }
    }

    mainLayout.center = new VBox {
      spacing = 10
      alignment = Pos.TopCenter
      children = Seq(
        gridPane,
        optionsComboBox,
        actionButton,
        isometryComboBox,
        useSectorCheckbox,
        sectorLabel,
        sectorGrid,
        pivotLabel,
        pivotGrid,
        optionsGrid,
        applyIsometryButton,
        saveButton,
        new HBox {
          spacing = 10
          alignment = Pos.Center
          children = Seq(
            new Button("Save Composed Isometry") {
              onAction = _ => saveComposedIsometry(expandingCheckbox, transparentCheckbox, isometryComboBox)
            }
          )
        }
      )
    }


    updateIsometryOptions()
  }

  private def promptForInt(message: String): Int = {
    val dialog = new TextInputDialog(defaultValue = "0") {
      title = "Input Required"
      headerText = message
    }
    dialog.showAndWait().map(_.toInt).getOrElse(0)
  }

  private def promptForDifficulty(): String = {
    val dialog = new ComboBox[String] {
      items = ObservableBuffer("Beginner", "Normal", "Advanced")
      promptText = "Choose difficulty"
      value = "Normal"
    }

    val resultDialog = new Alert(AlertType.Confirmation) {
      title = "Select Difficulty"
      headerText = "Choose difficulty level for validation:"
      contentText = "Select the difficulty level you want to validate against:"
    }

    resultDialog.dialogPane().setContent(dialog)

    val result = resultDialog.showAndWait()
    result match {
      case Some(ButtonType.OK) => dialog.value.value
      case _ => "Normal"
    }
  }


  private def showResults(): Unit = {
    val resultsText = if (bestResults.isEmpty) {
      "No results available yet."
    } else {
      bestResults
        .zipWithIndex
        .sortBy { case ((_, score), _) => -score }
        .map { case ((name, score), idx) =>
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


  private def saveComposedIsometry(expandingCheckbox: CheckBox, transparentCheckbox: CheckBox, isometryComboBox: ComboBox[String]): Unit = {
    val dialog = new TextInputDialog(defaultValue = "my_composition") {
      title = "Save Composed Isometry"
      headerText = "Enter a name for the composed isometry"
      contentText = "This will save the currently composed isometry with the given name"
    }

    dialog.showAndWait().foreach { name =>
      val composedIsometry = composeIsometries(expandingCheckbox, transparentCheckbox)
      composedIsometry.foreach { iso =>
        NamedIsometryRegistry.saveIsometry(name, iso)

        if (!MinesweeperApp.isometryOptions.contains(name)) {
          MinesweeperApp.isometryOptions = MinesweeperApp.isometryOptions :+ name
          isometryComboBox.items = ObservableBuffer(MinesweeperApp.isometryOptions: _*)
        }

        new Alert(AlertType.Information) {
          title = "Saved"
          headerText = s"Composed isometry '$name' saved successfully"
        }.showAndWait()
      }
    }
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