
import GridOperations._
import model.GameCell

import scala.annotation.tailrec
import scala.util.Random

import scala.collection.immutable.Queue

object GameLogic {


  def revealCellAndNeighbors(grid: Vector[Vector[GameCell]], startRow: Int, startCol: Int): Vector[Vector[GameCell]] = {

    @tailrec
    def revealRecursive(currentGrid: Vector[Vector[GameCell]], queue: Queue[(Int, Int)]): Vector[Vector[GameCell]] = {
      if (queue.isEmpty) {
        currentGrid
      } else {
        val ((row, col), remainingQueue) = queue.dequeue
        val cell = currentGrid(row)(col)

        if (cell.isRevealed || cell.isFlagged || cell.isMine) {
          revealRecursive(currentGrid, remainingQueue)
        } else {
          val gridWithRevealed = updateCell(currentGrid, row, col)(_.copy(isRevealed = true))

          val newQueue =
            if (cell.adjacentMines == 0) {
              val neighbors = getNeighbors(gridWithRevealed, row, col)
              val unrevealedNeighbors = neighbors.filter { case (nr, nc) =>
                val n = gridWithRevealed(nr)(nc)
                !n.isRevealed && !n.isFlagged
              }
              remainingQueue.enqueueAll(unrevealedNeighbors)
            } else {
              remainingQueue
            }

          revealRecursive(gridWithRevealed, newQueue)
        }
      }
    }

    revealRecursive(grid, Queue((startRow, startCol)))
  }


  //  def revealCellAndNeighbors(grid: Vector[Vector[GameCell]], startRow: Int, startCol: Int): Vector[Vector[GameCell]] = {
  //
  //    @tailrec
  //    def revealRecursive(currentGrid: Vector[Vector[GameCell]], queue: Queue[(Int, Int)], visited: Set[(Int, Int)]): Vector[Vector[GameCell]] = {
  //      if (queue.isEmpty) {
  //        currentGrid
  //      } else {
  //        val ((row, col), remainingQueue) = queue.dequeue
  //
  //        if (visited.contains((row, col))) {
  //          revealRecursive(currentGrid, remainingQueue, visited)
  //        } else {
  //          val cell = currentGrid(row)(col)
  //
  //          // skip if isRevealed/isFlagged/mine
  //          if (cell.isRevealed || cell.isFlagged || cell.isMine) {
  //            revealRecursive(currentGrid, remainingQueue, visited + ((row, col)))
  //          } else {
  //            // reveal
  //            val gridWithRevealed = updateCell(currentGrid, row, col)(_.copy(isRevealed = true))
  //
  //            val newQueue =
  //              if (cell.adjacentMines == 0) {
  //                val neighbors = getNeighbors(gridWithRevealed, row, col)
  //                val unrevealedNeighbors = neighbors.filter { case (nr, nc) =>
  //                  val neighborCell = gridWithRevealed(nr)(nc)
  //                  !visited.contains((nr, nc)) && !neighborCell.isRevealed && !neighborCell.isFlagged
  //                }
  //                remainingQueue.enqueueAll(unrevealedNeighbors)
  //              } else {
  //                remainingQueue
  //              }
  //
  //            revealRecursive(gridWithRevealed, newQueue, visited + ((row, col)))
  //          }
  //        }
  //      }
  //    }
  //
  //    revealRecursive(grid, Queue((startRow, startCol)), Set.empty)
  //  }


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

  def calculateFinalScore(state: GameState, isGameOver: Boolean = false): Option[(Long, Long, Int, Long)] = {
    if (state.levelCompleted) None
    else {
      val duration = java.time.Duration.between(state.startTime, java.time.Instant.now()).toSeconds
      val timePenalty = duration
      val clickPenalty = state.clickCount * 10

      val baseScore = if (isGameOver) 0L else state.score
      val finalScore = if (isGameOver) 0L else (state.score - timePenalty - clickPenalty).max(0)

      Some((baseScore, duration, state.clickCount, finalScore))
    }
  }


  // used for level creating

  def initGridWithoutMines(rows: Int, cols: Int): Vector[Vector[GameCell]] = {
    Vector.fill(rows, cols)(GameCell(isMine = false))
  }

}