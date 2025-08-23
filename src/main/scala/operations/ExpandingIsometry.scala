package operations

import model.GameCell


trait ExpandingIsometry extends Isometry {
  override def isExpanding: Boolean = true

  override protected def expandGrid(grid: Vector[Vector[GameCell]], mappedCoordinates: Seq[(Int, Int, Int, Int)]): (Vector[Vector[GameCell]], Int, Int) = {
    if (mappedCoordinates.isEmpty) return (grid, 0, 0)

    val currentRows = grid.length
    val currentCols = if (grid.nonEmpty) grid.head.length else 0

    if (currentRows == 0 || currentCols == 0) return (grid, 0, 0)

    //  min/max from target
    val targetRows = mappedCoordinates.map(_._3)
    val targetCols = mappedCoordinates.map(_._4)

    val minRow = targetRows.min
    val maxRow = targetRows.max
    val minCol = targetCols.min
    val maxCol = targetCols.max

    if (minRow >= 0 && minCol >= 0 && maxRow < currentRows && maxCol < currentCols) {
      // not needed
      return (grid, 0, 0)
    }

    val neededRows = maxRow + 1
    val neededCols = maxCol + 1

    // if negative we have to add
    val offsetRow = if (minRow < 0) -minRow else 0
    val offsetCol = if (minCol < 0) -minCol else 0

    //max(current, needed) + offset top/left
    val totalRows = math.max(currentRows, neededRows) + offsetRow
    val totalCols = math.max(currentCols, neededCols) + offsetCol

    val empty: GameCell = GameCell(false)

    val expandedGrid: Vector[Vector[GameCell]] =
      Vector.tabulate(totalRows, totalCols) { (row, col) =>
        val r0 = row - offsetRow
        val c0 = col - offsetCol
        // if in range of old, use it
        if (r0 >= 0 && r0 < currentRows && c0 >= 0 && c0 < currentCols)
          grid(r0)(c0)
        else {
          // means new space
          empty
        }
      }


    (expandedGrid, offsetRow, offsetCol)
  }
}