
import GridOperations._
import model.GameCell

import scala.annotation.tailrec
import scala.util.Random

object GameLogic {

  def revealCellAndNeighbors(grid: Vector[Vector[GameCell]], row: Int, col: Int): Vector[Vector[GameCell]] = {

    @tailrec
    def revealRecursive(currentGrid: Vector[Vector[GameCell]], toReveal: Set[(Int, Int)]): Vector[Vector[GameCell]] = {
      if (toReveal.isEmpty) currentGrid
      else {
        val r = toReveal.head._1
        val c = toReveal.head._2

        val cell = currentGrid(r)(c)

        if (cell.isRevealed || cell.isFlagged || cell.isMine) {
          revealRecursive(currentGrid, toReveal.tail)
        } else {
          val revealedGrid = updateCell(currentGrid, r, c)(_.copy(isRevealed = true))

          if (cell.adjacentMines == 0) {
            val neighbors = getNeighbors(revealedGrid, r, c)
            val newToReveal = toReveal.tail ++ neighbors.filter {
              case (nr, nc) => !revealedGrid(nr)(nc).isRevealed && !revealedGrid(nr)(nc).isFlagged
            }
            revealRecursive(revealedGrid, newToReveal)
          } else {
            revealRecursive(revealedGrid, toReveal.tail)
          }
        }
      }
    }

    revealRecursive(grid, Set((row, col)))
  }


  def revealAllMines(grid: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    grid.zipWithIndex.map {
      case (row, _) =>
        row.zipWithIndex.map {
          case (cell, _) =>
            if (cell.isMine)
              cell.copy(isRevealed = true)
            else
              cell
        }
    }
  }

  def suggestMove(state: GameState): Option[(Int, Int)] = {
    val unrevealedCells = for {
      (cells, row) <- state.grid.zipWithIndex
      (cell, col) <- cells.zipWithIndex
      if !cell.isRevealed && !cell.isMine && !state.suggested.contains((row, col)) && !cell.isFlagged
    } yield (row, col)

    if (unrevealedCells.nonEmpty)
      Some(unrevealedCells(Random.nextInt(unrevealedCells.size)))
    else
      None
  }

  def calculateFinalScore(
                           state: GameState,
                           isGameOver: Boolean = false
                         ): Option[(Long, Long, Int, Long)] = {
    if (state.levelCompleted) None
    else {
      val duration     = java.time.Duration.between(state.startTime, java.time.Instant.now()).toSeconds
      val timePenalty  = duration * 2
      val clickPenalty = state.clickCount * 10

      val baseScore  = if (isGameOver) 0L else state.score
      val finalScore = if (isGameOver) 0L else (state.score - timePenalty - clickPenalty).max(0)

      Some((baseScore, duration, state.clickCount, finalScore))
    }
  }


  // used for level creating

  def initGridWithoutMines(rows: Int, cols: Int): Vector[Vector[GameCell]] = {
    Vector.fill(rows, cols)(GameCell(isMine = false))
  }

}