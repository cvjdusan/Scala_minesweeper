import model.GameCell
import operations.Isometry

import java.time.Instant
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class GameController() {

  private var grid: Array[Array[GameCell]] = Array.ofDim[GameCell](0, 0)
  private var startTime: Instant            = Instant.EPOCH
  private var clickCount: Int               = 0
  private var levelCompleted: Boolean       = false
  private var score: Long                   = 1000
  private val suggested                     = mutable.Set.empty[(Int, Int)]

  private val MINE_SPACE = '#'

  // ---------------------------------------------------------------------------
  //  INIT / RESTART
  // ---------------------------------------------------------------------------

  def initGrid(rows: Int, columns: Int): Unit = {
    grid = Array.fill(rows, columns)(GameCell(isMine = false))
    clickCount = 0
    levelCompleted = false
  }

  def startGameTime(): Unit = {
    startTime = Instant.now()
  }

  def initScore(): Unit = {
    score = 1000
  }

  // ---------------------------------------------------------------------------
  //  METRICS
  // ---------------------------------------------------------------------------

  def incrementClickCount(): Unit = {
    clickCount += 1
  }

  def getClickCount: Int = clickCount

  def getScore: Long = score

  def decreaseScore(amount: Int): Unit = {
    score = math.max(0, score - amount)
  }

  def endGameSuccessfully(): Option[(Long, Long, Int, Long)] = {
    if (!levelCompleted) {
      levelCompleted = true
      val duration     = java.time.Duration.between(startTime, Instant.now()).toSeconds
      val timePenalty  = duration * 2
      val clickPenalty = clickCount * 10
      val finalScore   = (score - timePenalty - clickPenalty).max(0)
      Some((score, duration, clickCount, finalScore))
    } else {
      None
    }
  }

  // ---------------------------------------------------------------------------
  //  GRID HELPERS
  // ---------------------------------------------------------------------------

  private def calculateAdjacentMines(rows: Int, columns: Int): Unit = {
    for {
      row  <- 0 until rows
      col  <- 0 until columns
      cell =  grid(row)(col)
    } {
      if (!cell.isMine) {
        val count = countAdjacentMines(row, col, rows, columns)
        grid(row)(col) = cell.copy(adjacent = count)
      }
    }
  }

  private def countAdjacentMines(i: Int, j: Int, rows: Int, columns: Int): Int = {
    val coords = Seq(
      (i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
      (i    , j - 1),             (i    , j + 1),
      (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
    )
    coords.count { case (r, c) =>
      r >= 0 && r < rows && c >= 0 && c < columns && grid(r)(c).isMine
    }
  }

  private def getNeighbors(row: Int, col: Int): Seq[(Int, Int)] = {
    Seq(
      (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
      (row    , col - 1),                 (row    , col + 1),
      (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
    ).filter { case (r, c) =>
      r >= 0 && r < grid.length && c >= 0 && c < grid(0).length
    }
  }

  // ---------------------------------------------------------------------------
  //  PUBLIC GRID API
  // ---------------------------------------------------------------------------

  def checkIsMine(row: Int, col: Int): Boolean = grid(row)(col).isMine
  def getCell(row: Int, col: Int): GameCell    = grid(row)(col)
  def getGrid: Array[Array[GameCell]]          = grid
  def getMineCount(row: Int, col: Int): Int    = grid(row)(col).adjacent

  // ---------------------------------------------------------------------------
  //  PERSIST / LOAD
  // ---------------------------------------------------------------------------

  def loadGame(gridData: Array[String]): Unit = {
    val rows = gridData.length
    val cols = gridData.head.length
    initGrid(rows, cols)

    for {
      (line, r) <- gridData.zipWithIndex
      (ch, c)   <- line.zipWithIndex
    } {
      val cell = grid(r)(c)
      grid(r)(c) = cell.copy(isMine = ch == MINE_SPACE)
    }

    calculateAdjacentMines(rows, cols)
  }

  // ---------------------------------------------------------------------------
  //  GAMEPLAY
  // ---------------------------------------------------------------------------

  def revealCell(row: Int, col: Int): Unit = {
    if (row < 0 || row >= grid.length || col < 0 || col >= grid(row).length) return
    val cell = grid(row)(col)
    if (cell.isRevealed || cell.isMine) return

    grid(row)(col) = cell.copy(isRevealed = true)

    if (cell.adjacent > 0) return

    getNeighbors(row, col).foreach { case (r, c) => revealCell(r, c) }
  }

  def revealAllMines(): Unit = {
    for {
      row  <- grid.indices
      col  <- grid(row).indices
      cell =  grid(row)(col)
      if cell.isMine
    } {
      grid(row)(col) = cell.copy(isRevealed = true)
    }
  }

  // ---------------------------------------------------------------------------
  //  LEVEL EDIT / GENERATION
  // ---------------------------------------------------------------------------

  private def emptyWidth: Int = if (grid.isEmpty) 0 else grid(0).length

  def addRowBegin(): Unit = {
    val newRow = Array.fill(emptyWidth)(GameCell(false))
    grid = newRow +: grid
  }

  def addRowEnd(): Unit = {
    val newRow = Array.fill(emptyWidth)(GameCell(false))
    grid = grid :+ newRow
  }

  def addColumnBegin(): Unit = {
    grid = grid.map(row => GameCell(false) +: row)
  }

  def addColumnEnd(): Unit = {
    grid = grid.map(row => row :+ GameCell(false))
  }

  def removeRowBegin(): Unit    = if (grid.length > 1) grid = grid.tail
  def removeRowEnd(): Unit      = if (grid.length > 1) grid = grid.init
  def removeColumnBegin(): Unit = if (grid.head.length > 1) grid = grid.map(_.tail)
  def removeColumnEnd(): Unit   = if (grid.head.length > 1) grid = grid.map(_.init)

  def toggleCellType(row: Int, col: Int): Unit = {
    if (row >= 0 && row < grid.length && col >= 0 && col < grid(row).length) {
      val cell = grid(row)(col)
      grid(row)(col) = cell.copy(isMine = !cell.isMine)
    }
  }

  def clearSector(tlr: Int, tlc: Int, brr: Int, brc: Int): Unit = {
    for {
      r <- tlr to brr
      c <- tlc to brc
      if r >= 0 && r < grid.length && c >= 0 && c < grid(r).length
    } {
      val cell = grid(r)(c)
      grid(r)(c) = cell.copy(isMine = false)
    }
  }

  // ---------------------------------------------------------------------------
  //  HINTS
  // ---------------------------------------------------------------------------


  def suggestMove(): Option[(Int, Int)] = {
    val candidates = for {
      r    <- grid.indices
      c    <- grid(r).indices
      cell =  grid(r)(c)
      pos  =  (r, c)
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
  }

}
