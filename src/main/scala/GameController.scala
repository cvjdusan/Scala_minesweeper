import model.GameCell
import operations.{Isometry, LevelValidator, Sector}

import java.nio.file.Path
import java.time.Instant

class GameController() {

  // Partial functions
  val decreaseScoreBy10: GameState => GameState = _.decreaseScore(10)
  val decreaseScoreBy20: GameState => GameState = _.decreaseScore(20)
  val decreaseScoreBy5:  GameState => GameState = (gs: GameState) => gs.decreaseScore(5)

  def initGrid(r: Int, c: Int): GameState = {
    val newGrid = GameLogic.initGridWithoutMines(r, c)
    GameState.empty.copy(
      grid = newGrid,
      clickCount = 0,
      levelCompleted = false,
      score = 1000
    )
  }

  def startGameTime(state: GameState): GameState = {
    state.withStartTime(Instant.now())
  }

  def initScore(state: GameState): GameState = {
    state.withScore(1000)
  }

  def incrementClickCount(state: GameState): GameState = {
    state.incrementClickCount
  }

  def decreaseScore(state: GameState, amount: Int): GameState = {
    state.decreaseScore(amount)
  }

  def endGame(state: GameState): Option[(Long, Long, Int, Long)] = {
    val scoreWithState = GameLogic.calculateFinalScore(state).map { result =>
      (result, state.withLevelCompleted)
    }
    scoreWithState.map { case (result, _) => result }
  }

  def endGameGameOver(state: GameState): Option[(Long, Long, Int, Long)] = {
    val scoreWithState = GameLogic.calculateFinalScore(state, isGameOver = true).map { result =>
      (result, state.withLevelCompleted)
    }
    scoreWithState.map { case (result, _) => result }
  }

  def recalculateAdjacent(state: GameState): GameState = {
    val newGrid = GridOperations.recalculateAdjacent(state.grid)
    state.withGrid(newGrid)
  }

  def revealCellAndNeighbors(state: GameState, r: Int, c: Int): GameState = {
    val newGrid = GameLogic.revealCellAndNeighbors(state.grid, r, c)
    state.withGrid(newGrid)
  }

  def revealAllMines(state: GameState): GameState = {
    val newGrid = GameLogic.revealAllMines(state.grid)
    state.withGrid(newGrid)
  }

  def toggleMine(state: GameState, r: Int, c: Int): GameState = {
    import model.GameCellOps._
    val cell = state.grid(r)(c)
    val updatedCell = cell.unless(cell.isRevealed)(_.copy(isMine = !cell.isMine))
    val newGrid = GridOperations.updateCell(state.grid, r, c)(_ => updatedCell)
    state.withGrid(newGrid)
  }

  def toggleFlag(state: GameState, r: Int, c: Int): GameState = {
    import model.GameCellOps._
    val cell = state.grid(r)(c)
    val updatedCell = cell.when(!cell.isRevealed)(cell => cell.copy(isFlagged = !cell.isFlagged))
    val newGrid = GridOperations.updateCell(state.grid, r, c)(_ => updatedCell)
    state.withGrid(newGrid)
  }

  def suggestMove(state: GameState): (Option[(Int, Int)], GameState) = {
    GameLogic.suggestMove(state) match {
      case Some(move) =>
        val newState = decreaseScoreBy20(state.addSuggested(move))
        (Some(move), newState)
      case None => (None, state)
    }
  }

  def applyIsometry(state: GameState, isometry: Isometry): GameState = {
    val transformedGrid = isometry(state.grid)     //isometry.apply(state.grid)
    val recalculatedGrid = GridOperations.recalculateAdjacent(transformedGrid)
    state.withGrid(recalculatedGrid)
  }

  def applyIsometryToSector(state: GameState, iso: Isometry, sector: Sector, pivot: (Int, Int)): GameState = {
    val transformedGrid = iso.applyToSector(state.grid, sector, pivot)
    val recalculatedGrid = GridOperations.recalculateAdjacent(transformedGrid)
    state.withGrid(recalculatedGrid)
  }

  def createSector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): Sector = {
    Sector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)
  }

  def validateLevel(state: GameState, difficulty: String): Either[String, Boolean] = {
    LevelValidator.validateLevel(state.grid, difficulty)
  }

  def addRowBegin(state: GameState): GameState = {
    val emptyRow = Vector.fill(state.colsLength)(GameCell(false))
    val newGrid = emptyRow +: state.grid
    state.withGrid(newGrid)
  }

  def addRowEnd(state: GameState): GameState = {
    val emptyRow = Vector.fill(state.colsLength)(GameCell(false))
    val newGrid = state.grid :+ emptyRow
    state.withGrid(newGrid)
  }

  def addColumnBegin(state: GameState): GameState = {
    val newGrid = state.grid.map(row => GameCell(false) +: row)
    state.withGrid(newGrid)
  }

  def addColumnEnd(state: GameState): GameState = {
    val newGrid = state.grid.map(row => row :+ GameCell(false))
    state.withGrid(newGrid)
  }

  def removeRowBegin(state: GameState): GameState = {
    if (state.rowsLength > 1) {
      val newGrid = state.grid.tail
      state.withGrid(newGrid)
    } else state
  }

  def removeRowEnd(state: GameState): GameState = {
    if (state.rowsLength > 1) {
      val newGrid = state.grid.init
      state.withGrid(newGrid)
    } else state
  }

  def removeColumnBegin(state: GameState): GameState = {
    if (state.colsLength > 1) {
      val newGrid = state.grid.map(r => r.tail)
      state.withGrid(newGrid)
    } else state
  }

  def removeColumnEnd(state: GameState): GameState = {
    if (state.colsLength > 1) {
      val newGrid = state.grid.map(r => r.init)
      state.withGrid(newGrid)
    } else state
  }

  def clearSector(state: GameState, topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): GameState = {

    val rows = state.grid.length
    val cols = if (rows == 0) 0 else state.grid.head.length

    val r0 = math.max(0, topLeftRow)
    val r1 = math.min(rows - 1, bottomRightRow)
    val c0 = math.max(0, topLeftCol)
    val c1 = math.min(cols - 1, bottomRightCol)

    val newGrid =
      state.grid.zipWithIndex.map { case (rowVec, r) =>
        if (r >= r0 && r <= r1)
          rowVec.zipWithIndex.map { case (cell, c) =>
            if (c >= c0 && c <= c1) cell.copy(isMine = false) else cell
          }
        else rowVec
      }

    state.withGrid(newGrid)
  }


  def loadGame(state: GameState, lines: Array[String]): Either[String, GameState] = {
    GameIO.loadGame(lines).map { newGrid =>
      val recalculatedGrid = GridOperations.recalculateAdjacent(newGrid)
      state.withGrid(recalculatedGrid)
    }.left.map(_.toString)
  }

  def saveGame(state: GameState, path: Path): Either[String, Unit] = {
    GameIO.saveGame(state, path).left.map(_.toString)
  }

  def playMoves(state: GameState, seq: Seq[String]): GameState = {
    seq.foldLeft(state) {
      case (currentState, move) =>
        move match {
          case Move.Left(r, c) =>
            val cell = getCell(currentState, r - 1, c - 1)
            if (cell.isMine) revealAllMines(currentState)
            else revealCellAndNeighbors(currentState, r - 1, c - 1)

          case Move.Right(r, c) =>
            toggleFlag(currentState, r - 1, c - 1)

          case _ => currentState
        }
    }
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

  def getCell(state: GameState, r: Int, c: Int): GameCell = state.grid(r)(c)

  def getGrid(state: GameState): Vector[Vector[GameCell]] = state.grid

  def getScore(state: GameState): Long = state.score


  def getClickCount(state: GameState): Int = state.clickCount

  def isWin(state: GameState): Boolean =
    state.grid.flatten
      .filterNot(_.isMine)
      .forall(_.isRevealed)

  def rows(state: GameState): Int = state.rowsLength

  def cols(state: GameState): Int = state.colsLength

  def inBounds(state: GameState, r: Int, c: Int): Boolean = r >= 0 && r < state.rowsLength && c >= 0 && c < state.colsLength

}
