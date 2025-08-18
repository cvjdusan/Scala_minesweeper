import model.{GameCell}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.util.chaining.scalaUtilChainingOps

class GameLogicTest extends AnyFunSuite with Matchers {

  private def createTestGrid(rows: Int = 3, cols: Int = 3): Vector[Vector[GameCell]] = {
    Vector.fill(rows, cols)(GameCell(isMine = false))
  }

  private def createMineGrid(rows: Int = 3, cols: Int = 3, minePositions: Set[(Int, Int)]): Vector[Vector[GameCell]] = {
    val grid = createTestGrid(rows, cols)
    val gridWithMines = minePositions.foldLeft(grid) { case (g, (r, c)) =>
      GridOperations.toggleMine(g, r, c)
    }
    GridOperations.recalculateAdjacent(gridWithMines)
  }

  private def createGameState(minePositions: Set[(Int, Int)], rows: Int = 3, cols: Int = 3): GameState = {
    val grid = createMineGrid(rows, cols, minePositions)
    GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
  }

  test("GameLogic should initialize grid without mines correctly") {
    val grid = GameLogic.initGridWithoutMines(4, 5)
    
    grid.length shouldBe 4
    grid.head.length shouldBe 5
    grid.flatten.forall(!_.isMine) shouldBe true
    grid.flatten.forall(!_.isRevealed) shouldBe true
    grid.flatten.forall(!_.isFlagged) shouldBe true
    grid.flatten.forall(_.adjacentMines == 0) shouldBe true
  }

  test("grid operations - toggle mine") {
    val grid = GameLogic.initGridWithoutMines(3, 3)
    val withMine = GridOperations.toggleMine(grid, 1, 1)

    assert(withMine(1)(1).isMine)
    assert(!grid(1)(1).isMine)
  }

  test("grid operations - reveal cell") {
    val grid = GameLogic.initGridWithoutMines(3, 3)
    val revealed = GameLogic.revealCellAndNeighbors(grid, 0, 0)

    assert(revealed(0)(0).isRevealed)
    assert(!grid(0)(0).isRevealed)
  }

  test("grid operations - toggle flag") {
    val grid = GameLogic.initGridWithoutMines(3, 3)
    val flagged = GridOperations.toggleFlag(grid, 0, 0)

    assert(flagged(0)(0).isFlagged)
    assert(!grid(0)(0).isFlagged)
  }

  test("GameLogic should initialize empty grid correctly") {
    val grid = GameLogic.initGridWithoutMines(0, 0)
    
    grid shouldBe Vector.empty
  }

  test("GameLogic should initialize single cell grid correctly") {
    val grid = GameLogic.initGridWithoutMines(1, 1)
    
    grid.length shouldBe 1
    grid.head.length shouldBe 1
    grid(0)(0).isMine shouldBe false
  }

  test("GameLogic should reveal single cell correctly") {
    val grid = createMineGrid(3, 3, Set((1, 1)))
    val revealed = GameLogic.revealCellAndNeighbors(grid, 0, 0)
    
    revealed(0)(0).isRevealed shouldBe true
    grid(0)(0).isRevealed shouldBe false // original unchanged
  }

  test("GameLogic should reveal cell with adjacent mines correctly") {
    val grid = createMineGrid(3, 3, Set((0, 1), (1, 0)))
    val revealed = GameLogic.revealCellAndNeighbors(grid, 0, 0)
    
    revealed(0)(0).isRevealed shouldBe true
    // Should not reveal neighbors since adjacentMines > 0
    revealed(0)(1).isRevealed shouldBe false
    revealed(1)(0).isRevealed shouldBe false
  }

  test("GameLogic should not reveal flagged cells") {
    val grid = createMineGrid(3, 3, Set((1, 1)))
    val flaggedGrid = GridOperations.toggleFlag(grid, 0, 0)
    val revealed = GameLogic.revealCellAndNeighbors(flaggedGrid, 0, 1)
    
    revealed(0)(0).isRevealed shouldBe false // flagged cell not revealed
    revealed(0)(1).isRevealed shouldBe true
  }

  test("GameLogic should not reveal already revealed cells") {
    val grid = createMineGrid(3, 3, Set((1, 1)))
    val revealedGrid = GridOperations.updateCell(grid, 0, 0)(_.copy(isRevealed = true))
    val revealed = GameLogic.revealCellAndNeighbors(revealedGrid, 0, 0)
    
    revealed(0)(0).isRevealed shouldBe true
  }

  test("GameLogic should reveal all mines correctly") {
    val grid = createMineGrid(3, 3, Set((0, 0), (1, 1), (2, 2)))
    val revealed = GameLogic.revealAllMines(grid)
    
    revealed(0)(0).isRevealed shouldBe true
    revealed(1)(1).isRevealed shouldBe true
    revealed(2)(2).isRevealed shouldBe true
    revealed(0)(1).isRevealed shouldBe false // non-mine not revealed
    revealed(1)(0).isRevealed shouldBe false // non-mine not revealed
    
    grid(0)(0).isRevealed shouldBe false // original unchanged
  }

  test("GameLogic should handle reveal all mines on empty grid") {
    val grid = createTestGrid(3, 3)
    val revealed = GameLogic.revealAllMines(grid)
    
    revealed should equal(grid) // no changes
  }

  test("GameLogic should suggest move correctly") {
    val state = createGameState(Set((1, 1)))
    val suggestion = GameLogic.suggestMove(state)
    
    suggestion should not be empty
    val (row, col) = suggestion.get
    state.grid(row)(col).isMine shouldBe false
    state.grid(row)(col).isRevealed shouldBe false
    state.grid(row)(col).isFlagged shouldBe false
  }

  test("GameLogic should not suggest move for flagged cell") {
    val state = createGameState(Set((1, 1)))
    val flaggedState = state.withGrid(
      GridOperations.toggleFlag(state.grid, 0, 0)
    )
    val suggestion = GameLogic.suggestMove(flaggedState)
    
    suggestion should not be empty
    val (row, col) = suggestion.get
    (row, col) should not equal((0, 0)) // should not suggest flagged cell
  }

  test("GameLogic should not suggest move for already suggested cell") {
    val state = createGameState(Set((1, 1)))
    val suggestedState = state.addSuggested((0, 0))
    val suggestion = GameLogic.suggestMove(suggestedState)
    
    suggestion should not be empty
    val (row, col) = suggestion.get
    (row, col) should not equal((0, 0)) // should not suggest already suggested cell
  }

  test("GameLogic should return None when no valid moves available") {
    val grid = createMineGrid(2, 2, Set((0, 0)))
    // Reveal all non-mine cells
    val revealedGrid = GridOperations.updateCell(grid, 0, 1)(_.copy(isRevealed = true))
      .pipe(g => GridOperations.updateCell(g, 1, 0)(_.copy(isRevealed = true)))
      .pipe(g => GridOperations.updateCell(g, 1, 1)(_.copy(isRevealed = true)))
    
    val state = GameState(
      grid = revealedGrid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set((0, 1), (1, 0), (1, 1))
    )
    
    val suggestion = GameLogic.suggestMove(state)
    suggestion shouldBe None
  }


  test("GameLogic should calculate final score correctly") {
    val state = createGameState(Set((1, 1)))
    val startTime = Instant.now().minusSeconds(30) // 30 seconds ago
    val stateWithTime = state.copy(startTime = startTime, clickCount = 5)
    
    val result = GameLogic.calculateFinalScore(stateWithTime)
    
    result should not be empty
    val (initialScore, duration, clicks, finalScore) = result.get
    initialScore shouldBe 1000
    duration shouldBe 30
    clicks shouldBe 5
    finalScore shouldBe (1000 - 30 * 2 - 5 * 10).max(0) // 1000 - 60 - 50 = 890
  }

  test("GameLogic should not return negative final score") {
    val state = createGameState(Set((1, 1)))
    val startTime = Instant.now().minusSeconds(1000) // 1000 seconds ago
    val stateWithTime = state.copy(startTime = startTime, clickCount = 1000)
    
    val result = GameLogic.calculateFinalScore(stateWithTime)
    
    result should not be empty
    val (_, _, _, finalScore) = result.get
    finalScore shouldBe 0 // should not be negative
  }

  test("GameLogic should handle grid with mines on edges") {
    val grid = createMineGrid(3, 3, Set((0, 0), (0, 2), (2, 0), (2, 2)))
    val revealed = GameLogic.revealCellAndNeighbors(grid, 1, 1)
    
    revealed(1)(1).isRevealed shouldBe true
    // Center cell should be revealed since it has adjacent mines but is not a mine
    // Adjacent cells should not be revealed since center has adjacentMines > 0
    revealed(0)(1).isRevealed shouldBe false
    revealed(1)(0).isRevealed shouldBe false
    revealed(1)(2).isRevealed shouldBe false
    revealed(2)(1).isRevealed shouldBe false
  }
} 