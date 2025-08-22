import model.GameCell
import operations.{Isometry, Reflection, Rotation, Translation}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.util.chaining.scalaUtilChainingOps

class GameControllerTest extends AnyFunSuite with Matchers {

  private def createTestState(rows: Int = 3, cols: Int = 3): GameState = {
    val grid = Vector.fill(rows, cols)(GameCell(isMine = false))
    GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
  }

  private def createMineGrid(rows: Int = 3, cols: Int = 3, minePositions: Set[(Int, Int)]): GameState = {
    val grid = Vector.fill(rows, cols)(GameCell(isMine = false))
    val gridWithMines = minePositions.foldLeft(grid) { case (g, (r, c)) =>
      GridOperations.toggleMine(g, r, c)
    }
    val recalculatedGrid = GridOperations.recalculateAdjacent(gridWithMines)
    GameState(
      grid = recalculatedGrid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
  }

  test("GameController should initialize grid with correct dimensions") {
    val controller = new GameController()
    val state = controller.initGrid(5, 7)

    state.rowsLength shouldBe 5
    state.colsLength shouldBe 7
    state.grid.length shouldBe 5
    state.grid.head.length shouldBe 7
    state.clickCount shouldBe 0
    state.score shouldBe 1000
  }

  test("GameController should start game time correctly") {
    val controller = new GameController()
    val state = createTestState()
    val beforeTime = Instant.now()

    val newState = controller.startGameTime(state)

    newState.startTime should be >= beforeTime
    newState.startTime should be <= Instant.now()
  }

  test("GameController should increment click count correctly") {
    val controller = new GameController()
    val state = createTestState()

    val newState = controller.incrementClickCount(state)

    newState.clickCount shouldBe 1
    state.clickCount shouldBe 0 // original unchanged
  }

  test("GameController should decrease score correctly") {
    val controller = new GameController()
    val state = createTestState()

    val newState = controller.decreaseScore(state, 150)

    newState.score shouldBe 850
    state.score shouldBe 1000 // original unchanged
  }

  test("GameController should not decrease score below zero") {
    val controller = new GameController()
    val state = createTestState()

    val newState = controller.decreaseScore(state, 1500)

    newState.score shouldBe 0
  }

  test("GameController should reveal cell and neighbors correctly") {
    val controller = new GameController()
    val state = createMineGrid(3, 3, Set((1, 1)))

    val newState = controller.revealCellAndNeighbors(state, 0, 0)

    newState.grid(0)(0).isRevealed shouldBe true
    // Cell with mine should not be revealed
    newState.grid(1)(1).isRevealed shouldBe false
  }

  test("GameController should toggle mine correctly") {
    val controller = new GameController()
    val state = createTestState()

    val newState = controller.toggleMine(state, 1, 1)

    newState.grid(1)(1).isMine shouldBe true
    state.grid(1)(1).isMine shouldBe false // original unchanged
  }

  test("GameController should toggle flag correctly") {
    val controller = new GameController()
    val state = createTestState()

    val newState = controller.toggleFlag(state, 1, 1)

    newState.grid(1)(1).isFlagged shouldBe true
    state.grid(1)(1).isFlagged shouldBe false // original unchanged
  }

  test("GameController should not toggle flag on revealed cell") {
    val controller = new GameController()
    val state = createTestState()
    val revealedState = state.withGrid(
      GridOperations.updateCell(state.grid, 1, 1)(_.copy(isRevealed = true))
    )

    val newState = controller.toggleFlag(revealedState, 1, 1)

    newState.grid(1)(1).isFlagged shouldBe false
  }

  test("GameController should suggest move correctly") {
    val controller = new GameController()
    val state = createMineGrid(3, 3, Set((1, 1)))

    val (suggestion, newState) = controller.suggestMove(state)

    suggestion should not be empty
    val (row, col) = suggestion.get
    newState.grid(row)(col).isMine shouldBe false
    newState.grid(row)(col).isRevealed shouldBe false
    newState.score shouldBe 995 // decreased by 5 for hint
  }

  test("GameController should apply isometry correctly") {
    val controller = new GameController()
    // mine in upper left (0,0)
    val state = createMineGrid(3, 3, Set((0, 0)))
    val rotation = Rotation(clockwise = true)

    val newState = controller.applyIsometry(state, rotation)

    newState.grid should not equal state.grid

    newState.rowsLength shouldBe state.rowsLength
    newState.colsLength shouldBe state.colsLength

    // after rotation it shoudl be:
    // (0,0) -> (0, 2)
    state.grid(0)(0).isMine shouldBe true
    newState.grid(0)(2).isMine shouldBe true

    // and (0,0) cannot be mine
    newState.grid(0)(0).isMine shouldBe false
  }


  test("GameController should add row at beginning correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.addRowBegin(state)

    newState.rowsLength shouldBe 4
    newState.colsLength shouldBe 3

    newState.grid.last.forall(cell => !cell.isMine) shouldBe true
  }

  test("GameController should add row at end correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.addRowEnd(state)

    newState.rowsLength shouldBe 4
    newState.colsLength shouldBe 3

    newState.grid.last.forall(cell => !cell.isMine) shouldBe true
  }

  test("GameController should add column at beginning correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.addColumnBegin(state)

    newState.rowsLength shouldBe 3
    newState.colsLength shouldBe 4

    newState.grid.last.forall(cell => !cell.isMine) shouldBe true
  }

  test("GameController should add column at end correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.addColumnEnd(state)

    newState.rowsLength shouldBe 3
    newState.colsLength shouldBe 4
    newState.grid.forall(row => !row(3).isMine) shouldBe true
  }

  test("GameController should remove row at beginning correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.removeRowBegin(state)

    newState.rowsLength shouldBe 2
    newState.colsLength shouldBe 3
  }

  test("GameController should not remove row when only one row exists") {
    val controller = new GameController()
    val state = createTestState(1, 3)

    val newState = controller.removeRowBegin(state)

    newState.rowsLength shouldBe 1
    newState should equal(state)
  }

  test("GameController should remove row at end correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.removeRowEnd(state)

    newState.rowsLength shouldBe 2
    newState.colsLength shouldBe 3
  }

  test("GameController should remove column at beginning correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.removeColumnBegin(state)

    newState.rowsLength shouldBe 3
    newState.colsLength shouldBe 2
  }

  test("GameController should not remove column when only one column exists") {
    val controller = new GameController()
    val state = createTestState(3, 1)

    val newState = controller.removeColumnBegin(state)

    newState.colsLength shouldBe 1
    newState should equal(state)
  }

  test("GameController should remove column at end correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val newState = controller.removeColumnEnd(state)

    newState.rowsLength shouldBe 3
    newState.colsLength shouldBe 2
  }

  test("GameController should clear sector correctly") {
    val controller = new GameController()
    val state = createMineGrid(5, 5, Set((1, 1), (2, 2), (3, 3)))

    val newState = controller.clearSector(state, 1, 1, 3, 3)

    // All mines in sector should be cleared
    for {
      r <- 1 to 3
      c <- 1 to 3
    } {
      newState.grid(r)(c).isMine shouldBe false
    }
    // Mines outside sector should remain
    newState.grid(0)(0).isMine shouldBe false
    newState.grid(4)(4).isMine shouldBe false
  }

  test("GameController should check win condition correctly") {
    val controller = new GameController()
    val state = createMineGrid(2, 2, Set((0, 0)))
    val revealedState = state.withGrid(
      GridOperations.updateCell(state.grid, 0, 1)(_.copy(isRevealed = true))
        .pipe(g => GridOperations.updateCell(g, 1, 0)(_.copy(isRevealed = true)))
        .pipe(g => GridOperations.updateCell(g, 1, 1)(_.copy(isRevealed = true)))
    )

    controller.isWin(revealedState) shouldBe true
    controller.isWin(state) shouldBe false
  }

  test("GameController should check bounds correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    controller.inBounds(state, 0, 0) shouldBe true
    controller.inBounds(state, 2, 2) shouldBe true
    controller.inBounds(state, -1, 0) shouldBe false
    controller.inBounds(state, 0, -1) shouldBe false
    controller.inBounds(state, 3, 0) shouldBe false
    controller.inBounds(state, 0, 3) shouldBe false
  }

  test("GameController should get cell correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val cell = controller.getCell(state, 1, 1)

    cell shouldBe state.grid(1)(1)
  }

  test("GameController should get grid correctly") {
    val controller = new GameController()
    val state = createTestState(3, 3)

    val grid = controller.getGrid(state)

    grid should equal(state.grid)
  }

  test("GameController should get score correctly") {
    val controller = new GameController()
    val state = createTestState()

    val score = controller.getScore(state)

    score shouldBe 1000
  }

  test("GameController should get click count correctly") {
    val controller = new GameController()
    val state = createTestState()

    val clickCount = controller.getClickCount(state)

    clickCount shouldBe 0
  }

  test("GameController should get rows and cols correctly") {
    val controller = new GameController()
    val state = createTestState(5, 7)

    controller.rows(state) shouldBe 5
    controller.cols(state) shouldBe 7
  }
} 