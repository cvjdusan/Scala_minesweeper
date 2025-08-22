import model.{GameCell}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class ModelTest extends AnyFunSuite with Matchers {

  test("GameCell should create with default values correctly") {
    val cell = GameCell(isMine = false)
    
    cell.isMine shouldBe false
    cell.isRevealed shouldBe false
    cell.adjacentMines shouldBe 0
    cell.isFlagged shouldBe false
  }

  test("GameCell should create with custom values correctly") {
    val cell = GameCell(
      isMine = true,
      isRevealed = true,
      adjacentMines = 5,
      isFlagged = true
    )
    
    cell.isMine shouldBe true
    cell.isRevealed shouldBe true
    cell.adjacentMines shouldBe 5
    cell.isFlagged shouldBe true
  }

  test("GameCell should copy with modifications correctly") {
    val original = GameCell(isMine = false)
    val modified = original.copy(isRevealed = true, adjacentMines = 3)
    
    modified.isMine shouldBe false
    modified.isRevealed shouldBe true
    modified.adjacentMines shouldBe 3
    modified.isFlagged shouldBe false
    
    original.isRevealed shouldBe false // original unchanged
    original.adjacentMines shouldBe 0
  }

  test("GameCell should handle all boolean combinations") {
    val combinations = List(
      GameCell(isMine = false, isRevealed = false, isFlagged = false),
      GameCell(isMine = false, isRevealed = false, isFlagged = true),
      GameCell(isMine = false, isRevealed = true, isFlagged = false),
      GameCell(isMine = false, isRevealed = true, isFlagged = true),
      GameCell(isMine = true, isRevealed = false, isFlagged = false),
      GameCell(isMine = true, isRevealed = false, isFlagged = true),
      GameCell(isMine = true, isRevealed = true, isFlagged = false),
      GameCell(isMine = true, isRevealed = true, isFlagged = true)
    )
    
    combinations.length shouldBe 8
    combinations.forall(_.adjacentMines >= 0) shouldBe true
  }

  test("GameState should create with all parameters correctly") {
    val grid = Vector(Vector(GameCell(isMine = false)))
    val startTime = Instant.now()
    val state = GameState(
      grid = grid,
      startTime = startTime,
      clickCount = 5,
      levelCompleted = true,
      score = 750,
      suggested = Set((0, 0))
    )
    
    state.grid should equal(grid)
    state.startTime shouldBe startTime
    state.clickCount shouldBe 5
    state.levelCompleted shouldBe true
    state.score shouldBe 750
    state.suggested should contain((0, 0))
  }

  test("GameState should create empty state correctly") {
    val emptyState = GameState.empty
    
    emptyState.grid shouldBe Vector.empty
    emptyState.startTime shouldBe Instant.EPOCH
    emptyState.clickCount shouldBe 0
    emptyState.levelCompleted shouldBe false
    emptyState.score shouldBe 1000
    emptyState.suggested shouldBe Set.empty
  }

  test("GameState should calculate rows and cols correctly") {
    val grid = Vector(
      Vector(GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false))
    )
    val state = GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.rowsLength shouldBe 3
    state.colsLength shouldBe 2
  }

  test("GameState should handle empty grid dimensions") {
    val emptyState = GameState.empty
    
    emptyState.rowsLength shouldBe 0
    emptyState.colsLength shouldBe 0
  }

  test("GameState should handle single row grid dimensions") {
    val singleRowGrid = Vector(Vector(GameCell(isMine = false), GameCell(isMine = false)))
    val state = GameState(
      grid = singleRowGrid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.rowsLength shouldBe 1
    state.colsLength shouldBe 2
  }

  test("GameState should handle single column grid dimensions") {
    val singleColGrid = Vector(
      Vector(GameCell(isMine = false)),
      Vector(GameCell(isMine = false))
    )
    val state = GameState(
      grid = singleColGrid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.rowsLength shouldBe 2
    state.colsLength shouldBe 1
  }

  test("GameState should check game lost correctly") {
    val grid = Vector(
      Vector(GameCell(isMine = true, isRevealed = true), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false))
    )
    val state = GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.isGameLost shouldBe true
  }

  test("GameState should not report game lost when no mines are revealed") {
    val grid = Vector(
      Vector(GameCell(isMine = true, isRevealed = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false))
    )
    val state = GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.isGameLost shouldBe false
  }

  test("GameState should not report game lost when only non-mines are revealed") {
    val grid = Vector(
      Vector(GameCell(isMine = true, isRevealed = false), GameCell(isMine = false, isRevealed = true)),
      Vector(GameCell(isMine = false, isRevealed = true), GameCell(isMine = false))
    )
    val state = GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    state.isGameLost shouldBe false
  }

  test("GameState should withGrid correctly") {
    val originalGrid = Vector(Vector(GameCell(isMine = false)))
    val newGrid = Vector(Vector(GameCell(isMine = true)))
    val state = GameState(
      grid = originalGrid,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.withGrid(newGrid)
    
    newState.grid should equal(newGrid)
    newState.startTime shouldBe state.startTime
    newState.clickCount shouldBe state.clickCount
    newState.levelCompleted shouldBe state.levelCompleted
    newState.score shouldBe state.score
    newState.suggested should equal(state.suggested)
    
    state.grid should equal(originalGrid) // original unchanged
  }

  test("GameState should withStartTime correctly") {
    val originalTime = Instant.now()
    val newTime = originalTime.plusSeconds(60)
    val state = GameState(
      grid = Vector.empty,
      startTime = originalTime,
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.withStartTime(newTime)
    
    newState.startTime shouldBe newTime
    newState.grid should equal(state.grid)
    newState.clickCount shouldBe state.clickCount
    newState.levelCompleted shouldBe state.levelCompleted
    newState.score shouldBe state.score
    newState.suggested should equal(state.suggested)
    
    state.startTime shouldBe originalTime // original unchanged
  }

  test("GameState should incrementClickCount correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 5,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.incrementClickCount
    
    newState.clickCount shouldBe 6
    state.clickCount shouldBe 5 // original unchanged
  }

  test("GameState should withLevelCompleted correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.withLevelCompleted
    
    newState.levelCompleted shouldBe true
    state.levelCompleted shouldBe false // original unchanged
  }

  test("GameState should withScore correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.withScore(750)
    
    newState.score shouldBe 750
    state.score shouldBe 1000 // original unchanged
  }

  test("GameState should decreaseScore correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.decreaseScore(150)
    
    newState.score shouldBe 850
    state.score shouldBe 1000 // original unchanged
  }

  test("GameState should not decrease score below zero") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 100,
      suggested = Set.empty
    )
    
    val newState = state.decreaseScore(150)
    
    newState.score shouldBe 0
    state.score shouldBe 100 // original unchanged
  }

  test("GameState should addSuggested correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set((0, 0))
    )
    
    val newState = state.addSuggested((1, 1))
    
    newState.suggested should contain((0, 0))
    newState.suggested should contain((1, 1))
    state.suggested should contain only((0, 0)) // original unchanged
  }

  test("GameState should addSuggested to empty set") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state.addSuggested((0, 0))
    
    newState.suggested should contain only((0, 0))
    state.suggested shouldBe Set.empty // original unchanged
  }

  test("GameState should handle multiple suggested moves") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 0,
      levelCompleted = false,
      score = 1000,
      suggested = Set.empty
    )
    
    val newState = state
      .addSuggested((0, 0))
      .addSuggested((1, 1))
      .addSuggested((2, 2))
    
    newState.suggested should contain allOf((0, 0), (1, 1), (2, 2))
    newState.suggested.size shouldBe 3
    state.suggested shouldBe Set.empty // original unchanged
  }

  test("GameState should copy with modifications correctly") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = 5,
      levelCompleted = false,
      score = 750,
      suggested = Set((0, 0))
    )
    
    val newState = state.copy(
      clickCount = 10,
      score = 500,
      levelCompleted = true
    )
    
    newState.clickCount shouldBe 10
    newState.score shouldBe 500
    newState.levelCompleted shouldBe true
    newState.grid should equal(state.grid)
    newState.startTime shouldBe state.startTime
    newState.suggested should equal(state.suggested)
    
    state.clickCount shouldBe 5 // original unchanged
    state.score shouldBe 750
    state.levelCompleted shouldBe false
  }

  test("GameState should handle edge case with maximum values") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = Int.MaxValue,
      levelCompleted = true,
      score = Long.MaxValue,
      suggested = Set.empty
    )
    
    state.clickCount shouldBe Int.MaxValue
    state.score shouldBe Long.MaxValue
    state.levelCompleted shouldBe true
  }

  test("GameState should handle edge case with minimum values") {
    val state = GameState(
      grid = Vector.empty,
      startTime = Instant.now(),
      clickCount = Int.MinValue,
      levelCompleted = false,
      score = Long.MinValue,
      suggested = Set.empty
    )
    
    state.clickCount shouldBe Int.MinValue
    state.score shouldBe Long.MinValue
    state.levelCompleted shouldBe false
  }
} 