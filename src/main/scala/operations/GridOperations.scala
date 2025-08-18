import GridOperations.updateCell
import model.GameCell


object GridOperations {

  def updateCell(grid: Vector[Vector[GameCell]], row: Int, col: Int)(f: GameCell => GameCell): Vector[Vector[GameCell]] = {
    if (row >= 0 && row < grid.length && col >= 0 && col < grid.head.length) {
      grid.updated(row, grid(row).updated(col, f(grid(row)(col))))
    } else {
      grid
    }
  }

  def getCell(grid: Vector[Vector[GameCell]], row: Int, col: Int): Option[GameCell] = {
    if (row >= 0 && row < grid.length && col >= 0 && col < grid.head.length) {
      Some(grid(row)(col))
    } else {
      None
    }
  }

  def inBounds(grid: Vector[Vector[GameCell]], row: Int, col: Int): Boolean = {
    row >= 0 && row < grid.length && col >= 0 && col < grid.head.length
  }

  def getNeighbors(grid: Vector[Vector[GameCell]], row: Int, col: Int): Vector[(Int, Int)] = {
    val neighbors = Vector(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    )

    neighbors.collect {
      case (dr, dc) if inBounds(grid, row + dr, col + dc) => (row + dr, col + dc)
    }
  }

  def calculateAdjacentMines(grid: Vector[Vector[GameCell]], row: Int, col: Int): Int = {
    getNeighbors(grid, row, col).count { case (nr, nc) =>
      grid(nr)(nc).isMine
    }
  }

  def recalculateAdjacent(grid: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        if (cell.isMine) cell
        else cell.copy(adjacentMines = calculateAdjacentMines(grid, r, c))
      }
    }
  }

  def toggleMine(grid: Vector[Vector[GameCell]], row: Int, col: Int): Vector[Vector[GameCell]] = {
    updateCell(grid, row, col) { cell =>
      cell.copy(isMine = !cell.isMine)
    }
  }

  def toggleFlag(grid: Vector[Vector[GameCell]], row: Int, col: Int): Vector[Vector[GameCell]] = {
    updateCell(grid, row, col) { cell =>
      if (!cell.isRevealed) cell.copy(isFlagged = !cell.isFlagged)
      else cell
    }
  }

} 