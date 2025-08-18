import GameIO.{ParseError, ValidationError}
import model.GameCell
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.time.Instant

class GameIOTest extends AnyFunSuite with Matchers {

  private def createTestState(): GameState = {
    val grid = Vector(
      Vector(GameCell(isMine = true), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = true))
    )
    GameState(
      grid = grid,
      startTime = Instant.now(),
      clickCount = 5,
      levelCompleted = false,
      score = 800,
      suggested = Set((0, 1))
    )
  }

  test("GameIO should save game successfully") {
    val tempFile = Files.createTempFile("test-game", ".txt")
    val state = createTestState()

    val result = GameIO.saveGame(state, tempFile)

    assert(result.isRight)
    assert(Files.exists(tempFile))

    // Clean up
    Files.deleteIfExists(tempFile)
  }

  test("GameIO should load game successfully") {
    val lines = Array("#-", "-#")

    val result = GameIO.loadGame(lines)

    assert(result.isRight)
    val grid = result.right.get
    assert(grid.length == 2)
    assert(grid(0).length == 2)
    assert(grid(0)(0).isMine)
    assert(!grid(0)(1).isMine)
    assert(!grid(1)(0).isMine)
    assert(grid(1)(1).isMine)
  }

  test("GameIO should handle invalid character in load game") {
    val lines = Array("#X", "-#") // X is invalid

    val result = GameIO.loadGame(lines)

    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ParseError])
    }
  }

  test("GameIO should validate grid - empty grid") {
    val emptyGrid = Vector.empty[Vector[GameCell]]

    val result = GameIO.validateGrid(emptyGrid)

    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ValidationError])
    }
  }

  test("GameIO should validate grid - inconsistent row lengths") {
    val inconsistentGrid = Vector(
      Vector(GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false)) // shorter row
    )

    val result = GameIO.validateGrid(inconsistentGrid)

    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ValidationError])
    }
  }

  test("GameIO should validate grid - valid grid") {
    val validGrid = Vector(
      Vector(GameCell(isMine = true), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = true))
    )

    val result = GameIO.validateGrid(validGrid)

    assert(result.isRight)
  }

  test("GameIO should load game with different grid sizes") {
    val lines3x3 = Array("###", "---", "###")
    val result3x3 = GameIO.loadGame(lines3x3)
    
    assert(result3x3.isRight)
    val grid3x3 = result3x3.right.get
    grid3x3.length shouldBe 3
    grid3x3.head.length shouldBe 3
    grid3x3(0).forall(_.isMine) shouldBe true
    grid3x3(1).forall(!_.isMine) shouldBe true
    grid3x3(2).forall(_.isMine) shouldBe true
  }

  test("GameIO should load game with single row") {
    val lines1x3 = Array("-#-")
    val result = GameIO.loadGame(lines1x3)
    
    assert(result.isRight)
    val grid = result.right.get
    grid.length shouldBe 1
    grid.head.length shouldBe 3
    grid(0)(0).isMine shouldBe false
    grid(0)(1).isMine shouldBe true
    grid(0)(2).isMine shouldBe false
  }

  test("GameIO should load game with single column") {
    val lines3x1 = Array("#", "-", "#")
    val result = GameIO.loadGame(lines3x1)
    
    assert(result.isRight)
    val grid = result.right.get
    grid.length shouldBe 3
    grid.head.length shouldBe 1
    grid(0)(0).isMine shouldBe true
    grid(1)(0).isMine shouldBe false
    grid(2)(0).isMine shouldBe true
  }

  test("GameIO should handle empty lines array") {
    val emptyLines = Array.empty[String]
    val result = GameIO.loadGame(emptyLines)
    
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ValidationError])
    }
  }

  test("GameIO should handle lines with only whitespace") {
    val whitespaceLines = Array("   ", "  ", " ")
    val result = GameIO.loadGame(whitespaceLines)
    
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ValidationError])
    }
  }

  test("GameIO should handle lines with mixed valid and invalid characters") {
    val mixedLines = Array("#-", "X#") // X is invalid
    val result = GameIO.loadGame(mixedLines)
    
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[ParseError])
    }
  }

  test("GameIO should validate grid with single cell") {
    val singleCellGrid = Vector(Vector(GameCell(isMine = true)))
    val result = GameIO.validateGrid(singleCellGrid)
    
    assert(result.isRight)
  }

  test("GameIO should validate grid with rectangular shape") {
    val rectGrid = Vector(
      Vector(GameCell(isMine = false), GameCell(isMine = true), GameCell(isMine = false)),
      Vector(GameCell(isMine = true), GameCell(isMine = false), GameCell(isMine = true))
    )
    val result = GameIO.validateGrid(rectGrid)
    
    assert(result.isRight)
  }

  test("GameIO should validate grid with all mines") {
    val allMinesGrid = Vector(
      Vector(GameCell(isMine = true), GameCell(isMine = true)),
      Vector(GameCell(isMine = true), GameCell(isMine = true))
    )
    val result = GameIO.validateGrid(allMinesGrid)
    
    assert(result.isRight)
  }

  test("GameIO should validate grid with no mines") {
    val noMinesGrid = Vector(
      Vector(GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false))
    )
    val result = GameIO.validateGrid(noMinesGrid)
    
    assert(result.isRight)
  }

  test("GameIO should handle very large grid") {
    val largeGrid = Vector.fill(100, 100)(GameCell(isMine = false))
    val result = GameIO.validateGrid(largeGrid)
    
    assert(result.isRight)
    largeGrid.length shouldBe 100
    largeGrid.head.length shouldBe 100
  }

  test("GameIO should handle grid with maximum dimensions") {
    val maxGrid = Vector.fill(1000, 1000)(GameCell(isMine = false))
    val result = GameIO.validateGrid(maxGrid)
    
    assert(result.isRight)
    maxGrid.length shouldBe 1000
    maxGrid.head.length shouldBe 1000
  }

  test("GameIO should preserve cell properties during save and load") {
    val tempFile = Files.createTempFile("test-game", ".txt")
    val state = createTestState()
    
    // Save game
    val saveResult = GameIO.saveGame(state, tempFile)
    assert(saveResult.isRight)
    
    // Read file content to verify format
    val lines = Files.readAllLines(tempFile).toArray(Array.empty[String])
    assert(lines.length == 2)
    assert(lines(0) == "#-")
    assert(lines(1) == "-#")
    
    // Load game and verify
    val loadResult = GameIO.loadGame(lines)
    assert(loadResult.isRight)
    val loadedGrid = loadResult.right.get
    
    // Verify grid structure
    loadedGrid.length shouldBe 2
    loadedGrid.head.length shouldBe 2
    loadedGrid(0)(0).isMine shouldBe true
    loadedGrid(0)(1).isMine shouldBe false
    loadedGrid(1)(0).isMine shouldBe false
    loadedGrid(1)(1).isMine shouldBe true
    
    // Clean up
    Files.deleteIfExists(tempFile)
  }

  test("GameIO should handle file with trailing empty lines") {
    val linesWithTrailing = Array("#-", "-#", "", "  ")
    val result = GameIO.loadGame(linesWithTrailing)
    
    assert(result.isRight)
    val grid = result.right.get
    grid.length shouldBe 2
    grid.head.length shouldBe 2
  }

  test("GameIO should handle file with leading empty lines") {
    val linesWithLeading = Array("", "  ", "#-", "-#")
    val result = GameIO.loadGame(linesWithLeading)
    
    assert(result.isRight)
    val grid = result.right.get
    grid.length shouldBe 2
    grid.head.length shouldBe 2
  }

}