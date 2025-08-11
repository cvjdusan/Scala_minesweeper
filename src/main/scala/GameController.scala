import model.GameCell
import operations.Isometry

import java.nio.file.Path
import java.time.Instant


class GameController() {

  // ---------------------------------------------------------------------------

  private var gameState: GameState = GameState.empty

  private def grid: Vector[Vector[GameCell]] = gameState.grid

  private def score: Long = gameState.score

  private val MineChar = '#'
  private val FlagChar = 'F'
  private val RevealChar = 'R'
  private val HiddenChar = '-'

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------
  private def rows: Int = gameState.rows

  private def cols: Int = gameState.cols

  def isWin: Boolean = grid.flatten.forall(cell => cell.isRevealed || cell.isMine)


  // ---------------------------------------------------------------------------
  //  INIT
  // ---------------------------------------------------------------------------

  def initGrid(r: Int, c: Int): Unit = {
    val newGrid = GameLogic.initGridWithoutMines(r, c)
    gameState = gameState.copy(
      grid = newGrid,
      clickCount = 0,
      levelCompleted = false,
      score = 1000
    )
  }

  def startGameTime(): Unit = {
    gameState = gameState.withStartTime(Instant.now())
  }

  def initScore(): Unit = {
    gameState = gameState.withScore(1000)
  }

  // ---------------------------------------------------------------------------
  //  METRICS
  // ---------------------------------------------------------------------------
  def incrementClickCount(): Unit = {
    gameState = gameState.incrementClickCount
  }

  def getScore: Long = score

  def decreaseScore(amount: Int): Unit = {
    gameState = gameState.decreaseScore(amount)
  }

  def endGame(): Option[(Long, Long, Int, Long)] = {
    GameLogic.calculateFinalScore(gameState).map { result =>
      gameState = gameState.withLevelCompleted
      result
    }
  }

  // ---------------------------------------------------------------------------
  //  NEIGHBOURS & ADJACENT COUNT
  // ---------------------------------------------------------------------------

  def recalculateAdjacent(): Unit = {
    val newGrid = GridOperations.recalculateAdjacent(grid)
    gameState = gameState.withGrid(newGrid)
  }

  // ---------------------------------------------------------------------------
  //  PUBLIC ACCESSORS
  // ---------------------------------------------------------------------------

  def getCell(r: Int, c: Int): GameCell = grid(r)(c)

  def getGrid: Vector[Vector[GameCell]] = grid

  // ---------------------------------------------------------------------------
  //  LOAD/SAVE
  // ---------------------------------------------------------------------------
  def saveGame(path: Path): Unit = {
    GameIO.saveGame(gameState, path) match {
      case Right(_) => // Success - do nothing
      case Left(error) => throw new RuntimeException(s"Failed to save game: $error")
    }
  }

  def loadGame(lines: Array[String]): Unit = {
    GameIO.loadGame(lines) match {
      case Right(newGrid) =>
        val recalculatedGrid = GridOperations.recalculateAdjacent(newGrid)
        gameState = gameState.withGrid(recalculatedGrid)
      case Left(error) =>
        throw new RuntimeException(s"Failed to load game: $error")
    }
  }

  def playMoves(seq: Seq[String]): Unit = seq.foreach {
    case Move.Left(r, c) => revealCellAndNeigboursWithoutMines(r - 1, c - 1)
    case Move.Right(r, c) => toggleFlag(r - 1, c - 1)
    case _ => ()
  }

  private object Move {
    private val L = """L\((\d+),(\d+)\)""".r
    private val D = """D\((\d+),(\d+)\)""".r

    object Left {
      def unapply(s: String): Option[(Int, Int)] = s match {
        case L(x, y) => Some((x.toInt, y.toInt))
        case _ => None
      }
    }

    object Right {
      def unapply(s: String): Option[(Int, Int)] = s match {
        case D(x, y) => Some((x.toInt, y.toInt))
        case _ => None
      }
    }
  }

  // ---------------------------------------------------------------------------
  //  GAMEPLAY
  // ---------------------------------------------------------------------------

  def revealCellAndNeigboursWithoutMines(r: Int, c: Int): Unit = {
    val newGrid = GameLogic.revealCellAndNeighbors(grid, r, c)
    gameState = gameState.withGrid(newGrid)
  }

  def revealAllMines(): Unit = {
    val newGrid = GameLogic.revealAllMines(grid)
    gameState = gameState.withGrid(newGrid)
  }

  // ---------------------------------------------------------------------------
  //  LEVEL EDITING
  // ---------------------------------------------------------------------------
  private def emptyRow: Vector[GameCell] = Vector.fill(cols)(GameCell(false))

  // row / column operations
  def addRowBegin(): Unit = {
    val newGrid = emptyRow +: grid
    gameState = gameState.withGrid(newGrid)
  }

  def addRowEnd(): Unit = {
    val newGrid = grid :+ emptyRow
    gameState = gameState.withGrid(newGrid)
  }

  def addColumnBegin(): Unit = {
    val newGrid = grid.map(row => GameCell(false) +: row)
    gameState = gameState.withGrid(newGrid)
  }

  def addColumnEnd(): Unit = {
    val newGrid = grid.map(row => row :+ GameCell(false))
    gameState = gameState.withGrid(newGrid)
  }

  def removeRowBegin(): Unit = if (rows > 1) {
    val newGrid = grid.tail
    gameState = gameState.withGrid(newGrid)
  }

  def removeRowEnd(): Unit = if (rows > 1) {
    val newGrid = grid.init
    gameState = gameState.withGrid(newGrid)
  }

  def removeColumnBegin(): Unit = if (cols > 1) {
    val newGrid = grid.map(_.tail)
    gameState = gameState.withGrid(newGrid)
  }

  def removeColumnEnd(): Unit = if (cols > 1) {
    val newGrid = grid.map(_.init)
    gameState = gameState.withGrid(newGrid)
  }

  // used for creating levels
  def toggleMine(r: Int, c: Int): Unit = {
    val newGrid = GridOperations.toggleMine(grid, r, c)
    gameState = gameState.withGrid(newGrid)
  }

  def toggleFlag(r: Int, c: Int): Unit = {
    val newGrid = GridOperations.toggleFlag(grid, r, c)
    gameState = gameState.withGrid(newGrid)
  }

  def clearSector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): Unit = {
    var newGrid = grid
    for {
      r <- topLeftRow to bottomRightRow
      c <- topLeftCol to bottomRightCol
      if GridOperations.inBounds(newGrid, r, c)
    } {
      newGrid = GridOperations.updateCell(newGrid, r, c)(_.copy(isMine = false))
    }
    gameState = gameState.withGrid(newGrid)
  }

  // ---------------------------------------------------------------------------
  //  HINTS
  // ---------------------------------------------------------------------------
  def suggestMove(): Option[(Int, Int)] = {
    GameLogic.suggestMove(gameState) match {
      case Some(move) =>
        gameState = gameState.addSuggested(move).decreaseScore(5)
        Some(move)
      case None => None
    }
  }

  // ---------------------------------------------------------------------------
  //  ISOMETRY
  // ---------------------------------------------------------------------------
  def applyIsometry(iso: Isometry): Unit = {
    val transformedGrid = iso(grid)
    val recalculatedGrid = GridOperations.recalculateAdjacent(transformedGrid)
    gameState = gameState.withGrid(recalculatedGrid)
  }

}
