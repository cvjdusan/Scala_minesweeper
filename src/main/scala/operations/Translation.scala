package operations
import model.GameCell

case class Translation(dx: Int, dy: Int) extends Isometry {

  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    val rows = grid.length
    val cols = if (rows > 0) grid(0).length else 0
    val translated = Array.ofDim[GameCell](rows + math.abs(dy), cols + math.abs(dx))

    for (row <- grid.indices; col <- grid(row).indices) {
      val newRow = row + dy
      val newCol = col + dx
      if (newRow >= 0 && newRow < translated.length && newCol >= 0 && newCol < translated(0).length) {
        translated(newRow)(newCol) = grid(row)(col)
      }
    }

    translated.map(row => row.map(cell => if (cell == null) GameCell(isMine = false) else cell))
  }
}
