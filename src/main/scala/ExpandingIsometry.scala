case class ExpandingIsometry(isometry: Isometry) extends Isometry {
  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    val transformed = isometry(grid)
    val maxRows = Math.max(grid.length, transformed.length)
    val maxCols = Math.max(grid.head.length, transformed.head.length)

    val expandedOriginal = expandGrid(grid, maxRows, maxCols)
    val expandedTransformed = expandGrid(transformed, maxRows, maxCols)

    expandedOriginal.zip(expandedTransformed).map { case (row1, row2) =>
      row1.zip(row2).map { case (cell1, cell2) =>
        if (cell2.isMine) cell2 else cell1
      }
    }
  }

  private def expandGrid(grid: Array[Array[GameCell]], rows: Int, cols: Int): Array[Array[GameCell]] = {
    val emptyRow = Array.fill(cols)(GameCell(isMine = false))
    val expanded = grid.map(row => row ++ Array.fill(cols - row.length)(GameCell(isMine = false)))
    expanded ++ Array.fill(rows - expanded.length)(emptyRow)
  }
}
