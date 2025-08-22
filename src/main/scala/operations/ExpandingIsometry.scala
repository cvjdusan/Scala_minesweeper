package operations

import model.GameCell


trait ExpandingIsometry extends Isometry {
  override def isExpanding: Boolean = true

  override protected def expandGrid[A](grid: Vector[Vector[A]], mappedCoordinates: Seq[(Int, Int, Int, Int)]): (Vector[Vector[A]], Int, Int) = {
    if (mappedCoordinates.isEmpty) return (grid, 0, 0)

    val currentRows = grid.length
    val currentCols = if (grid.nonEmpty) grid.head.length else 0

    if (currentRows == 0 || currentCols == 0) return (grid, 0, 0)

    //  min/max koordinate iz mapiranja
    val targetRows = mappedCoordinates.map(_._3)
    val targetCols = mappedCoordinates.map(_._4)

    val minRow = targetRows.min
    val maxRow = targetRows.max
    val minCol = targetCols.min
    val maxCol = targetCols.max

    // izr potrebne dimenzije
    val totalRows = math.max(currentRows, maxRow + 1) - math.min(0, minRow)
    val totalCols = math.max(currentCols, maxCol + 1) - math.min(0, minCol)

    // izr offset za pozicioniranje originalnog grida
    val offsetRow = if (minRow < 0) -minRow else 0
    val offsetCol = if (minCol < 0) -minCol else 0

    // Kreiraj prazan grid
    val empty = grid(0)(0) match {
      case _: GameCell => GameCell(false).asInstanceOf[A]
      case other => other
    }

    val expandedGrid = Vector.tabulate(totalRows) { row =>
      Vector.tabulate(totalCols) { col =>
        // Originalni grid se pomera za offset
        val originalRow = row - offsetRow
        val originalCol = col - offsetCol
        if (originalRow >= 0 && originalRow < currentRows &&
          originalCol >= 0 && originalCol < currentCols) {
          grid(originalRow)(originalCol)
        } else {
          empty
        }
      }
    }

    (expandedGrid, offsetRow, offsetCol)
  }
}