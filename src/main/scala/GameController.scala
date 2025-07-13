import model.GameCell
import operations.Isometry

import java.time.Instant
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class GameController() {

  // ---------------------------------------------------------------------------
  //  STATE
  // ---------------------------------------------------------------------------
  private var grid: Vector[Vector[GameCell]] = Vector.empty
  private var startTime: Instant = Instant.EPOCH
  private var clickCount: Int = 0
  private var levelCompleted: Boolean = false
  private var score: Long = 1000
  private val suggested = mutable.Set.empty[(Int, Int)]

  private val MineChar = '#'

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------
  private def rows: Int = grid.length

  private def cols: Int = if (grid.isEmpty) 0 else grid.head.length

  private def inBounds(r: Int, c: Int): Boolean =
    r >= 0 && r < rows && c >= 0 && c < cols

  private def cellAt(r: Int, c: Int): Option[GameCell] =
    if (inBounds(r, c)) Some(grid(r)(c)) else None

  private def update(r: Int, c: Int)(f: GameCell => GameCell): Unit =
    grid = grid.updated(r, grid(r).updated(c, f(grid(r)(c))))

  // ---------------------------------------------------------------------------
  //  INIT
  // ---------------------------------------------------------------------------
  def initGrid(r: Int, c: Int): Unit = {
    grid = Vector.fill(r, c)(GameCell(isMine = false))
    clickCount = 0
    levelCompleted = false
  }

  def startGameTime(): Unit = startTime = Instant.now()

  def initScore(): Unit = score = 1000

  // ---------------------------------------------------------------------------
  //  METRICS
  // ---------------------------------------------------------------------------
  def incrementClickCount(): Unit = clickCount += 1

  def getClickCount: Int = clickCount

  def getScore: Long = score

  def decreaseScore(amount: Int): Unit = score = math.max(0, score - amount)

  def endGameSuccessfully(): Option[(Long, Long, Int, Long)] =
    if (!levelCompleted) {
      levelCompleted = true
      val duration = java.time.Duration.between(startTime, Instant.now()).toSeconds
      val timePenalty = duration * 2
      val clickPenalty = clickCount * 10
      val finalScore = (score - timePenalty - clickPenalty).max(0)
      Some((score, duration, clickCount, finalScore))
    } else None

  // ---------------------------------------------------------------------------
  //  NEIGHBOURS & ADJACENT COUNT
  // ---------------------------------------------------------------------------
  private def neighbours(r: Int, c: Int): Seq[(Int, Int)] =
    for {
      dr <- -1 to 1
      dc <- -1 to 1
      if !(dr == 0 && dc == 0)
      nr = r + dr; nc = c + dc
      if inBounds(nr, nc)
    } yield (nr, nc)

  private def recalculateAdjacent(): Unit =
    grid = grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        if (cell.isMine) cell
        else cell.copy(adjacent = neighbours(r, c).count { case (nr, nc) => grid(nr)(nc).isMine })
      }
    }

  // ---------------------------------------------------------------------------
  //  PUBLIC ACCESSORS
  // ---------------------------------------------------------------------------
  def checkIsMine(r: Int, c: Int): Boolean = grid(r)(c).isMine

  def getCell(r: Int, c: Int): GameCell = grid(r)(c)

  def getGrid: Vector[Vector[GameCell]] = grid

  def getMineCount(r: Int, c: Int): Int = grid(r)(c).adjacent

  // ---------------------------------------------------------------------------
  //  LOAD/SAVE
  // ---------------------------------------------------------------------------
  def loadGame(lines: Array[String]): Unit = {
    val r = lines.length
    val c = lines.headOption.map(_.length).getOrElse(0)
    initGrid(r, c)
    grid = grid.zipWithIndex.map { case (_, rowIdx) =>
      Vector.tabulate(c) { colIdx =>
        GameCell(isMine = lines(rowIdx)(colIdx) == MineChar)
      }
    }
    recalculateAdjacent()
  }

  // ---------------------------------------------------------------------------
  //  GAMEPLAY
  // ---------------------------------------------------------------------------
  def revealCell(r: Int, c: Int): Unit = cellAt(r, c) match {
    case Some(cell) if !cell.isRevealed && !cell.isMine =>
      update(r, c)(_.copy(isRevealed = true))
      if (cell.adjacent == 0)
        neighbours(r, c).foreach { case (nr, nc) => revealCell(nr, nc) }
    case _ => ()
  }

  def revealAllMines(): Unit =
    for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c).isMine
    } update(r, c)(_.copy(isRevealed = true))

  // ---------------------------------------------------------------------------
  //  LEVEL EDITING
  // ---------------------------------------------------------------------------
  private def emptyRow: Vector[GameCell] = Vector.fill(cols)(GameCell(false))

  // row / column operations
  def addRowBegin(): Unit = grid = emptyRow +: grid

  def addRowEnd(): Unit = grid = grid :+ emptyRow

  def addColumnBegin(): Unit = grid = grid.map(row => GameCell(false) +: row)

  def addColumnEnd(): Unit = grid = grid.map(row => row :+ GameCell(false))

  def removeRowBegin(): Unit = if (rows > 1) grid = grid.tail

  def removeRowEnd(): Unit = if (rows > 1) grid = grid.init

  def removeColumnBegin(): Unit = if (cols > 1) grid = grid.map(_.tail)

  def removeColumnEnd(): Unit = if (cols > 1) grid = grid.map(_.init)

  def toggleCellType(r: Int, c: Int): Unit =
    cellAt(r, c).foreach(cell => update(r, c)(_.copy(isMine = !cell.isMine)))

  def clearSector(tlr: Int, tlc: Int, brr: Int, brc: Int): Unit =
    for {
      r <- tlr to brr
      c <- tlc to brc
      if inBounds(r, c)
    } update(r, c)(_.copy(isMine = false))

  // ---------------------------------------------------------------------------
  //  HINTS
  // ---------------------------------------------------------------------------
  def suggestMove(): Option[(Int, Int)] = {
    val candidates = for {
      r <- grid.indices
      c <- grid(r).indices
      pos = (r, c)
      cell = grid(r)(c)
      if !cell.isRevealed && !cell.isMine && !suggested(pos)
    } yield pos

    if (candidates.isEmpty) None
    else {
      val pick = candidates(Random.nextInt(candidates.size))
      suggested += pick
      Some(pick)
    }
  }

  // ---------------------------------------------------------------------------
  //  ISOMETRY
  // ---------------------------------------------------------------------------
  def applyIsometry(iso: Isometry): Unit = {
    grid = iso(grid)
    recalculateAdjacent()
  }

}
