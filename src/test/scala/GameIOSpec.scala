//import model.GameCell
//import org.scalatest.funsuite.AnyFunSuite
//
//import java.nio.file.{Files, Path}
//import java.time.Instant
//
//class GameIOSpec extends AnyFunSuite {
//
//  private def createTestState(): GameState = {
//    val grid = Vector(
//      Vector(GameCell(isMine = true), GameCell(isMine = false)),
//      Vector(GameCell(isMine = false), GameCell(isMine = true))
//    )
//    GameState(
//      grid = grid,
//      startTime = Instant.now(),
//      clickCount = 5,
//      levelCompleted = false,
//      score = 800,
//      suggested = Set((0, 1))
//    )
//  }
//
//  test("functional save game - success") {
//    val tempFile = Files.createTempFile("test-game", ".txt")
//    val state = createTestState()
//
//    val result = GameIO.saveGame(state, tempFile)
//
//    assert(result.isRight)
//    assert(Files.exists(tempFile))
//
//    // Clean up
//    Files.deleteIfExists(tempFile)
//  }
//
//  test("functional load game - success") {
//    val lines = Array("#-", "-#")
//
//    val result = GameIO.loadGame(lines)
//
//    assert(result.isRight)
//    val grid = result.right.get
//    assert(grid.length == 2)
//    assert(grid(0).length == 2)
//    assert(grid(0)(0).isMine)
//    assert(!grid(0)(1).isMine)
//    assert(!grid(1)(0).isMine)
//    assert(grid(1)(1).isMine)
//  }
//
//  test("functional load game - invalid character") {
//    val lines = Array("#X", "-#") // X is invalid
//
//    val result = GameIO.loadGame(lines)
//
//    assert(result.isLeft)
//    result.left.foreach { error =>
//      assert(error.isInstanceOf[ParseError])
//      assert(error.message.contains("Invalid character"))
//    }
//  }
//
//  test("functional validate grid - empty grid") {
//    val emptyGrid = Vector.empty[Vector[GameCell]]
//
//    val result = GameIO.validateGrid(emptyGrid)
//
//    assert(result.isLeft)
//    result.left.foreach { error =>
//      assert(error.isInstanceOf[ValidationError])
//      assert(error.message.contains("empty"))
//    }
//  }
//
//  test("functional validate grid - inconsistent row lengths") {
//    val inconsistentGrid = Vector(
//      Vector(GameCell(isMine = false), GameCell(isMine = false)),
//      Vector(GameCell(isMine = false)) // shorter row
//    )
//
//    val result = GameIO.validateGrid(inconsistentGrid)
//
//    assert(result.isLeft)
//    result.left.foreach { error =>
//      assert(error.isInstanceOf[ValidationError])
//      assert(error.message.contains("same length"))
//    }
//  }
//
//  test("functional validate grid - valid grid") {
//    val validGrid = Vector(
//      Vector(GameCell(isMine = true), GameCell(isMine = false)),
//      Vector(GameCell(isMine = false), GameCell(isMine = true))
//    )
//
//    val result = GameIO.validateGrid(validGrid)
//
//    assert(result.isRight)
//  }
//
//  test("functional load game from file - complete flow") {
//    val tempFile = Files.createTempFile("test-load", ".txt")
//
//    try {
//      // Write test data
//      Files.write(tempFile, java.util.Arrays.asList("#-", "-#"))
//
//      val result = GameIO.loadGameFromFile(tempFile)
//
//      assert(result.isRight)
//      val grid = result.right.get
//      assert(grid.length == 2)
//      assert(grid(0)(0).isMine)
//      assert(grid(1)(1).isMine)
//
//    } finally {
//      Files.deleteIfExists(tempFile)
//    }
//  }
//
//  test("functional error handling - file not found") {
//    val nonExistentFile = Files.get("non-existent-file.txt")
//
//    val result = GameIO.loadGameFromFile(nonExistentFile)
//
//    assert(result.isLeft)
//    result.left.foreach { error =>
//      assert(error.isInstanceOf[FileError])
//    }
//  }
//
//  test("functional error composition with for-comprehension") {
//    val tempFile = Files.createTempFile("test-composition", ".txt")
//
//    try {
//      // Write invalid data
//      Files.write(tempFile, java.util.Arrays.asList("#X", "-#"))
//
//      val result = for {
//        lines <- GameIO.readGameFile(tempFile)
//        grid <- GameIO.loadGame(lines)
//        _ <- GameIO.validateGrid(grid)
//      } yield grid
//
//      assert(result.isLeft)
//      result.left.foreach { error =>
//        assert(error.isInstanceOf[ParseError])
//      }
//
//    } finally {
//      Files.deleteIfExists(tempFile)
//    }
//  }
//}