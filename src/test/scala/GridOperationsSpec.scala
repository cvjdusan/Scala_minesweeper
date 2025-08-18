import model.GameCell
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GridOperationsSpec extends AnyFunSuite with Matchers {

  private def createTestGrid(rows: Int = 3, cols: Int = 3): Vector[Vector[GameCell]] = {
    Vector.fill(rows, cols)(GameCell(isMine = false))
  }

  private def createMineGrid(rows: Int = 3, cols: Int = 3, minePositions: Set[(Int, Int)]): Vector[Vector[GameCell]] = {
    val grid = createTestGrid(rows, cols)
    minePositions.foldLeft(grid) { case (g, (r, c)) =>
      GridOperations.toggleMine(g, r, c)
    }
  }

  test("GridOperations should update cell correctly") {
    val grid = createTestGrid(3, 3)
    val updatedGrid = GridOperations.updateCell(grid, 1, 1)(_.copy(isMine = true))
    
    updatedGrid(1)(1).isMine shouldBe true
    grid(1)(1).isMine shouldBe false // original unchanged
  }

  test("GridOperations should not update cell outside bounds") {
    val grid = createTestGrid(3, 3)
    val updatedGrid = GridOperations.updateCell(grid, 5, 5)(_.copy(isMine = true))
    
    updatedGrid should equal(grid) // no change
  }

  test("GridOperations should get cell correctly when in bounds") {
    val grid = createTestGrid(3, 3)
    val cell = GridOperations.getCell(grid, 1, 1)
    
    cell shouldBe Some(grid(1)(1))
  }

  test("GridOperations should return None for cell outside bounds") {
    val grid = createTestGrid(3, 3)
    val cell = GridOperations.getCell(grid, 5, 5)
    
    cell shouldBe None
  }

  test("GridOperations should check bounds correctly") {
    val grid = createTestGrid(3, 3)
    
    GridOperations.inBounds(grid, 0, 0) shouldBe true
    GridOperations.inBounds(grid, 2, 2) shouldBe true
    GridOperations.inBounds(grid, -1, 0) shouldBe false
    GridOperations.inBounds(grid, 0, -1) shouldBe false
    GridOperations.inBounds(grid, 3, 0) shouldBe false
    GridOperations.inBounds(grid, 0, 3) shouldBe false
  }

  test("GridOperations should get neighbors correctly for center cell") {
    val grid = createTestGrid(3, 3)
    val neighbors = GridOperations.getNeighbors(grid, 1, 1)
    
    neighbors should contain allOf((0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2))
    neighbors.length shouldBe 8
  }

  test("GridOperations should get neighbors correctly for corner cell") {
    val grid = createTestGrid(3, 3)
    val neighbors = GridOperations.getNeighbors(grid, 0, 0)
    
    neighbors should contain allOf((0, 1), (1, 0), (1, 1))
    neighbors.length shouldBe 3
  }

  test("GridOperations should get neighbors correctly for edge cell") {
    val grid = createTestGrid(3, 3)
    val neighbors = GridOperations.getNeighbors(grid, 0, 1)
    
    neighbors should contain allOf((0, 0), (0, 2), (1, 0), (1, 1), (1, 2))
    neighbors.length shouldBe 5
  }

  test("GridOperations should calculate adjacent mines correctly") {
    val grid = createMineGrid(3, 3, Set((0, 0), (0, 1), (2, 2)))
    val adjacentMines = GridOperations.calculateAdjacentMines(grid, 1, 1)
    
    adjacentMines shouldBe 3 // (0,0), (0,1), (2,2) are adjacent to (1,1)
  }

  test("GridOperations should calculate adjacent mines for corner cell") {
    val grid = createMineGrid(3, 3, Set((0, 1), (1, 0)))
    val adjacentMines = GridOperations.calculateAdjacentMines(grid, 0, 0)
    
    adjacentMines shouldBe 2
  }

  test("GridOperations should calculate adjacent mines for edge cell") {
    val grid = createMineGrid(3, 3, Set((0, 0), (0, 2), (1, 1)))
    val adjacentMines = GridOperations.calculateAdjacentMines(grid, 0, 1)
    
    adjacentMines shouldBe 3
  }

  test("GridOperations should calculate adjacent mines for cell with no adjacent mines") {
    val grid = createMineGrid(3, 3, Set((0, 0), (2, 2)))
    val adjacentMines = GridOperations.calculateAdjacentMines(grid, 1, 1)
    
    adjacentMines shouldBe 2
  }

  test("GridOperations should recalculate adjacent mines for entire grid") {
    val grid = createMineGrid(3, 3, Set((0, 0), (0, 1), (2, 2)))
    val recalculatedGrid = GridOperations.recalculateAdjacent(grid)

    // Non-mine cells
    recalculatedGrid(1)(1).adjacentMines shouldBe 3
    recalculatedGrid(0)(2).adjacentMines shouldBe 1
    recalculatedGrid(1)(0).adjacentMines shouldBe 2
    recalculatedGrid(1)(2).adjacentMines shouldBe 2
    recalculatedGrid(2)(0).adjacentMines shouldBe 0
    recalculatedGrid(2)(1).adjacentMines shouldBe 1

    // Mines remain mines
    recalculatedGrid(0)(0).isMine shouldBe true
    recalculatedGrid(0)(1).isMine shouldBe true
    recalculatedGrid(2)(2).isMine shouldBe true
  }


  test("GridOperations should toggle mine correctly") {
    val grid = createTestGrid(3, 3)
    val toggledGrid = GridOperations.toggleMine(grid, 1, 1)
    
    toggledGrid(1)(1).isMine shouldBe true
    grid(1)(1).isMine shouldBe false // original unchanged
  }

  test("GridOperations should toggle mine back to false") {
    val grid = createMineGrid(3, 3, Set((1, 1)))
    val toggledGrid = GridOperations.toggleMine(grid, 1, 1)
    
    toggledGrid(1)(1).isMine shouldBe false
    grid(1)(1).isMine shouldBe true // original unchanged
  }

  test("GridOperations should toggle flag correctly for unrevealed cell") {
    val grid = createTestGrid(3, 3)
    val flaggedGrid = GridOperations.toggleFlag(grid, 1, 1)
    
    flaggedGrid(1)(1).isFlagged shouldBe true
    grid(1)(1).isFlagged shouldBe false // original unchanged
  }

  test("GridOperations should toggle flag back to false") {
    val grid = createTestGrid(3, 3)
    val flaggedGrid = GridOperations.toggleFlag(grid, 1, 1)
    val unflaggedGrid = GridOperations.toggleFlag(flaggedGrid, 1, 1)
    
    unflaggedGrid(1)(1).isFlagged shouldBe false
  }

  test("GridOperations should not toggle flag for revealed cell") {
    val grid = createTestGrid(3, 3)
    val revealedGrid = GridOperations.updateCell(grid, 1, 1)(_.copy(isRevealed = true))
    val flaggedGrid = GridOperations.toggleFlag(revealedGrid, 1, 1)
    
    flaggedGrid(1)(1).isFlagged shouldBe false
    revealedGrid(1)(1).isFlagged shouldBe false
  }

  test("GridOperations should handle empty grid correctly") {
    val emptyGrid = Vector.empty[Vector[GameCell]]
    
    GridOperations.inBounds(emptyGrid, 0, 0) shouldBe false
    GridOperations.getCell(emptyGrid, 0, 0) shouldBe None
    GridOperations.getNeighbors(emptyGrid, 0, 0) shouldBe Vector.empty
  }

  test("GridOperations should handle single cell grid correctly") {
    val singleCellGrid = Vector(Vector(GameCell(isMine = true)))
    
    GridOperations.inBounds(singleCellGrid, 0, 0) shouldBe true
    GridOperations.inBounds(singleCellGrid, 0, 1) shouldBe false
    GridOperations.inBounds(singleCellGrid, 1, 0) shouldBe false
    
    val neighbors = GridOperations.getNeighbors(singleCellGrid, 0, 0)
    neighbors shouldBe Vector.empty
  }

  test("GridOperations should handle rectangular grid correctly") {
    val rectGrid = createTestGrid(2, 4)
    
    rectGrid.length shouldBe 2
    rectGrid.head.length shouldBe 4
    
    GridOperations.inBounds(rectGrid, 1, 3) shouldBe true
    GridOperations.inBounds(rectGrid, 1, 4) shouldBe false
    GridOperations.inBounds(rectGrid, 2, 3) shouldBe false
  }
} 