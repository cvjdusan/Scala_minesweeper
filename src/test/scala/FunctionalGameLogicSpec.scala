import model.GameCell
import org.scalatest.funsuite.AnyFunSuite

import java.time.Instant

class FunctionalGameLogicSpec extends AnyFunSuite {

  private def create3x3Grid(mineAt: (Int, Int)): Vector[Vector[GameCell]] = {
    val grid = GameLogic.initGridWithoutMines(3, 3)
    val withMine = GridOperations.toggleMine(grid, mineAt._1, mineAt._2)
    GridOperations.recalculateAdjacent(withMine)
  }

  private def createGameState(mineAt: (Int, Int)): GameState = {
    val grid = create3x3Grid(mineAt)
    GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
  }

  test("functional grid operations - toggle mine") {
    val grid = GameLogic.initGridWithoutMines(3, 3)
    val withMine = GridOperations.toggleMine(grid, 1, 1)
    
    assert(withMine(1)(1).isMine)
    assert(!grid(1)(1).isMine) // original grid unchanged
  }

  test("functional grid operations - reveal cell") {
    val grid = create3x3Grid((1, 1))
    val revealed = GameLogic.revealCellAndNeighbors(grid, 0, 0)
    
    assert(revealed(0)(0).isRevealed)
    assert(!grid(0)(0).isRevealed) // original grid unchanged
  }

  test("functional grid operations - toggle flag") {
    val grid = create3x3Grid((1, 1))
    val flagged = GridOperations.toggleFlag(grid, 0, 0)
    
    assert(flagged(0)(0).isFlagged)
    assert(!grid(0)(0).isFlagged) // original grid unchanged
  }

  test("functional game state - increment click count") {
    val state = createGameState((1, 1))
    val newState = state.incrementClickCount
    
    assert(newState.clickCount == 1)
    assert(state.clickCount == 0) // original state unchanged
  }

  test("functional game state - decrease score") {
    val state = createGameState((1, 1))
    val newState = state.decreaseScore(100)
    
    assert(newState.score == 900)
    assert(state.score == 1000) // original state unchanged
  }

  test("functional suggest move") {
    val state = createGameState((1, 1))
    val suggestion = GameLogic.suggestMove(state)
    
    assert(suggestion.isDefined)
    val (row, col) = suggestion.get
    assert(!state.grid(row)(col).isMine)
    assert(!state.grid(row)(col).isRevealed)
  }

  test("functional game won check") {
    val grid = create3x3Grid((1, 1))
    
    // Reveal all non-mine cells
    val revealed = (for {
      r <- grid.indices
      c <- grid(r).indices
      if !grid(r)(c).isMine
    } yield (r, c)).foldLeft(grid) { case (currentGrid, (r, c)) =>
      GridOperations.updateCell(currentGrid, r, c)(_.copy(isRevealed = true))
    }
    
    assert(GameLogic.isGameWon(revealed))
    assert(!GameLogic.isGameWon(grid))
  }

  test("functional reveal all mines") {
    val grid = create3x3Grid((1, 1))
    val revealed = GameLogic.revealAllMines(grid)
    
    assert(revealed(1)(1).isRevealed) // mine is revealed
    assert(!revealed(0)(0).isRevealed) // non-mine not revealed
    assert(!grid(1)(1).isRevealed) // original unchanged
  }

  test("functional flood fill reveal") {
    val grid = create3x3Grid((2, 2)) // mine at bottom-right
    val revealed = GameLogic.revealCellAndNeighbors(grid, 0, 0) // top-left has adjacent == 0
    
    assert(revealed(0)(0).isRevealed)
    assert(revealed(1)(0).isRevealed) // neighbor also revealed
    assert(!revealed(2)(2).isRevealed) // mine still hidden
    assert(!grid(0)(0).isRevealed) // original unchanged
  }

  test("functional calculate final score") {
    val state = createGameState((1, 1))
    val result = GameLogic.calculateFinalScore(state)
    
    assert(result.isDefined)
    val (initialScore, duration, clicks, finalScore) = result.get
    assert(initialScore == 1000)
    assert(clicks == 0)
    assert(finalScore <= initialScore) // should be less due to time penalty
  }

  test("functional game state immutability chain") {
    val state = createGameState((1, 1))
    
    val finalState = state
      .incrementClickCount
      .decreaseScore(50)
      .addSuggested((0, 0))
      .withLevelCompleted
    
    // Original state unchanged
    assert(state.clickCount == 0)
    assert(state.score == 1000)
    assert(state.suggested.isEmpty)
    assert(!state.levelCompleted)
    
    // Final state has all changes
    assert(finalState.clickCount == 1)
    assert(finalState.score == 950)
    assert(finalState.suggested.contains((0, 0)))
    assert(finalState.levelCompleted)
  }
}