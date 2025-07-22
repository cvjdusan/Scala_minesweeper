import model.GameCell
import operations.Isometry

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsJava}
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
  private val FlagChar = 'F'
  private val RevealChar = 'R'
  private val HiddenChar = '-'

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------
  private def rows: Int = grid.length

  private def cols: Int = if (grid.isEmpty) 0 else grid.head.length

  private def inBounds(r: Int, c: Int): Boolean =
    r >= 0 && r < rows && c >= 0 && c < cols

  private def cellAt(r: Int, c: Int): Option[GameCell] =
    if (inBounds(r, c)) Some(grid(r)(c)) else None

  private def updateCellAt(r: Int, c: Int)(f: GameCell => GameCell): Unit =
    grid = grid
      .updated(r, grid(r)
        .updated(c, f(grid(r)(c))))

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

  private val neighboursVector = Vector(
    (-1,-1), (-1,0), (-1,1),
    (0,-1),         (0,1),
    (1,-1), (1,0),  (1,1)
  )

  private def getNeighbours(r: Int, c: Int): Vector[(Int, Int)] =
    neighboursVector.collect {
      case (dr, dc)
        if inBounds(r + dr, c + dc) => (r + dr, c + dc)
    }

  def recalculateAdjacent(): Unit =
    grid = grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        if (cell.isMine) cell
        else cell.copy(adjacentMines = getNeighbours(r, c).count { case (nr, nc) => grid(nr)(nc).isMine })
      }
    }

  // ---------------------------------------------------------------------------
  //  PUBLIC ACCESSORS
  // ---------------------------------------------------------------------------
  def checkIsMine(r: Int, c: Int): Boolean = grid(r)(c).isMine

  def getCell(r: Int, c: Int): GameCell = grid(r)(c)

  def getGrid: Vector[Vector[GameCell]] = grid

  def getMineCount(r: Int, c: Int): Int = grid(r)(c).adjacentMines

  // ---------------------------------------------------------------------------
  //  LOAD/SAVE
  // ---------------------------------------------------------------------------
  def saveGame(path: Path): Unit = {
    val lines = grid.map(_.map {
      case GameCell(true, _, _, _) => MineChar
      case GameCell(_, _, _, true) => FlagChar
      case GameCell(_, rev, _, _) if rev => RevealChar
      case _ => HiddenChar
    }.mkString)
    Files.write(path, lines.asJava)
  }

  def loadGame(lines: Array[String]): Unit = loadLines(lines.toSeq)

  def loadLines(lines: Seq[String]): Unit = {
    val r = lines.length
    val c = lines.headOption.map(_.length).getOrElse(0)
    initGrid(r, c)

    grid = lines.zipWithIndex.map { case (rowStr, ri) =>
      rowStr.zipWithIndex.map { case (ch, ci) => ch match {
        case `MineChar` => GameCell(isMine = true)
        case `FlagChar` => GameCell(isMine = false, isFlagged = true)
        case `RevealChar` => GameCell(isMine = false, isRevealed = true)
        case _ => GameCell(isMine = false)

      }
      }.toVector
    }.toVector

    recalculateAdjacent()
  }

  def playMoves(seq: Seq[String]): Unit = seq.foreach {
    case Move.Left(r, c) => revealCellAndNeigboursWithoutMines(r - 1, c - 1)
    case Move.Right(r, c) => toggleFlag(r - 1, c - 1)
    case _ => ()
  }

  private object Move {
    private val L = """L\((\d+),(\d+)\)""".r
    private val D = """D\((\d+),(\d+)\)""".r

    object Left {
      def unapply(s: String): Option[(Int, Int)] = s match {
        case L(x, y) => Some((x.toInt, y.toInt))
        case _ => None
      }
    }

    object Right {
      def unapply(s: String): Option[(Int, Int)] = s match {
        case D(x, y) => Some((x.toInt, y.toInt))
        case _ => None
      }
    }
  }


  // ---------------------------------------------------------------------------
  //  GAMEPLAY
  // ---------------------------------------------------------------------------
  def revealCellAndNeigboursWithoutMines(r: Int, c: Int): Unit = cellAt(r, c) match {
    case Some(cell) if !cell.isRevealed && !cell.isMine && !cell.isFlagged =>
      updateCellAt(r, c)(_.copy(isRevealed = true)) //cell => cell.copy(isRevealed = true)
      if (cell.adjacentMines == 0)
        getNeighbours(r, c).foreach { case (nr, nc) => revealCellAndNeigboursWithoutMines(nr, nc) }
    case _ => ()
  }

  def revealAllMines(): Unit =
    for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c).isMine
    } updateCellAt(r, c)(_.copy(isRevealed = true))

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

  // used for creating levels
  def toggleMine(r: Int, c: Int): Unit =
    cellAt(r, c).foreach(cell => updateCellAt(r, c)(_.copy(isMine = !cell.isMine)))

  def toggleFlag(r: Int, c: Int): Unit = cellAt(r, c).foreach(cell =>
    updateCellAt(r, c)(_.copy(isFlagged = !cell.isFlagged))
  )

  def clearSector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): Unit =
    for {
      r <- topLeftRow to bottomRightRow
      c <- topLeftCol to bottomRightCol
      if inBounds(r, c)
    } updateCellAt(r, c)(_.copy(isMine = false))

  // ---------------------------------------------------------------------------
  //  HINTS
  // ---------------------------------------------------------------------------
  def suggestMove(): Option[(Int, Int)] = {
    val candidates = for {
      r <- grid.indices
      c <- grid(r).indices
      pos = (r, c)
      cell = grid(r)(c)
      if !cell.isRevealed && !cell.isFlagged && !cell.isMine && !suggested(pos)
    } yield pos

    if (candidates.isEmpty) None
    else {
      val pick = candidates(Random.nextInt(candidates.size))
      suggested += pick
      decreaseScore(5)
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
