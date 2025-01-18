case class Rotation(clockwise: Boolean = true) extends Isometry {

  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    val rows = grid.length
    val cols = if (rows > 0) grid(0).length else 0
    val rotated = Array.ofDim[GameCell](cols, rows)

    for (row <- 0 until rows; col <- 0 until cols) {
      if (clockwise) {
        rotated(col)(rows - 1 - row) = grid(row)(col) // Rows are now cols
      } else {
        rotated(cols - 1 - col)(row) = grid(row)(col) // Cols are now rows
      }
    }

    rotated
  }
}
