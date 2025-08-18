import model.GameCell
import operations.Isometry

import java.nio.file.Path
import java.time.Instant

class GameController() {

  // Parcijalno primenjene funkcije - funkcionalne konstrukcije
  val decreaseScoreBy10: GameState => GameState = _.decreaseScore(10)
  val decreaseScoreBy20: GameState => GameState = _.decreaseScore(20)
  val decreaseScoreBy5: GameState => GameState = _.decreaseScore(5)

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
    val updatedCell = cell.when(!cell.isRevealed)(_.copy(isFlagged = !cell.isFlagged))
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

  def applyIsometry(state: GameState, iso: Isometry): GameState = {
    val transformedGrid = iso(state.grid)
    val recalculatedGrid = GridOperations.recalculateAdjacent(transformedGrid)
    state.withGrid(recalculatedGrid)
  }

  def addRowBegin(state: GameState): GameState = {
    val emptyRow = Vector.fill(state.cols)(GameCell(false))
    val newGrid = emptyRow +: state.grid
    state.withGrid(newGrid)
  }

  def addRowEnd(state: GameState): GameState = {
    val emptyRow = Vector.fill(state.cols)(GameCell(false))
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
    if (state.rows > 1) {
      val newGrid = state.grid.tail
      state.withGrid(newGrid)
    } else state
  }

  def removeRowEnd(state: GameState): GameState = {
    if (state.rows > 1) {
      val newGrid = state.grid.init
      state.withGrid(newGrid)
    } else state
  }

  def removeColumnBegin(state: GameState): GameState = {
    if (state.cols > 1) {
      val newGrid = state.grid.map(_.tail)
      state.withGrid(newGrid)
    } else state
  }

  def removeColumnEnd(state: GameState): GameState = {
    if (state.cols > 1) {
      val newGrid = state.grid.map(_.init)
      state.withGrid(newGrid)
    } else state
  }

  def clearSector(state: GameState, topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): GameState = {
    val newGrid = (topLeftRow to bottomRightRow).foldLeft(state.grid) { case (grid, r) =>
      (topLeftCol to bottomRightCol).foldLeft(grid) { case (g, c) =>
        if (GridOperations.inBounds(g, r, c)) {
          GridOperations.updateCell(g, r, c)(_.copy(isMine = false))
        } else g
      }
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
  def rows(state: GameState): Int = state.rows
  def cols(state: GameState): Int = state.cols
  def inBounds(state: GameState, r: Int, c: Int): Boolean = r >= 0 && r < state.rows && c >= 0 && c < state.cols

}
