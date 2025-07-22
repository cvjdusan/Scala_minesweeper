import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.jdk.CollectionConverters.CollectionHasAsScala

class GameControllerSpec extends AnyFunSuite {

  private def create3x3(mineAt: (Int, Int)): GameController = {
    val gc = new GameController()
    gc.initGrid(3, 3)
    gc.toggleMine(mineAt._1, mineAt._2)
    gc.recalculateAdjacent()
    gc
  }

  test("left click reveals non‑mine cell") {
    val gc = create3x3((1, 1))
    gc.revealCellAndNeigboursWithoutMines(0, 0)
    assert(gc.getCell(0, 0).isRevealed)
  }

  // TODO: Fix bug
  test("right click toggles flag") {
    val gc = create3x3((1, 1))
    gc.toggleFlag(0, 0)
    assert(gc.getCell(0, 0).isFlagged)
    gc.toggleFlag(0, 0)
    assert(!gc.getCell(0, 0).isFlagged)
  }

  test("hint suggests unrevealed non‑mine cell and remembers it") {
    val gc = create3x3((1, 1))
    val first = gc.suggestMove().get
    assert(!gc.getCell(first._1, first._2).isMine)
    val second = gc.suggestMove().get
    assert(first != second) // not the same suggestion twice
  }

  test("left click reveals safe cell and floods when adjacent == 0") {
    val gc = create3x3((2,2))           // mine bottom‑right
    gc.revealCellAndNeigboursWithoutMines(0,0)                 // top‑left has adjacent == 0
    assert(gc.getCell(0,0).isRevealed)
    assert(gc.getCell(1,0).isRevealed) // neighbour also zero
    assert(!gc.getCell(2,2).isRevealed) // mine still hidden
  }

  test("revealAllMines marks only mines revealed") {
    val gc = create3x3((0,2))
    gc.revealAllMines()
    assert(gc.getCell(0,2).isRevealed)
    assert(!gc.getCell(0,0).isRevealed)
  }

  test("save then load restores identical grid") {
    val tmp = Files.createTempFile("grid",".txt")
    val gc1 = create3x3((1,1))
    gc1.saveGame(tmp)

    val gc2 = new GameController()
    val lines = Files.readAllLines(tmp).asScala.toArray
    gc2.loadGame(lines)

    assert(gc1.getGrid == gc2.getGrid)
  }

  test("playMoves replays left/right clicks from file") {
    val gc = create3x3((2,2))
    gc.playMoves(Seq("L(1,1)", "D(1,2)", "L(3,3)") )
    assert(gc.getCell(0,0).isRevealed)
    assert(gc.getCell(0,1).isFlagged)
    assert(gc.getCell(2,2).isMine)  // mine clicked
  }

  test("score decreases after hint") {
    val gc = create3x3((1,1))
    val beforeScore = gc.getScore
    gc.suggestMove()
    assert(gc.getScore < beforeScore)
  }

}
