case class TransparentIsometry(isometry: Rotation) {

  def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    val transformed = isometry(grid)
    val rows = grid.length
    val cols = if (rows > 0) grid(0).length else 0

    val result = Array.ofDim[GameCell](rows, cols)

    for (row <- 0 until rows; col <- 0 until cols) {
      result(row)(col) =
        if (row < transformed.length && col < transformed(row).length && transformed(row)(col).isMine)
          transformed(row)(col)
        else
          grid(row)(col)
    }

    result
  }
}
