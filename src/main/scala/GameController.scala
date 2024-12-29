class GameController(gridSize: Int, numMines: Int) {

  private val grid: Array[Array[Cell]] = Array.ofDim[Cell](gridSize, gridSize)


  def initializeGame(): Unit = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      grid(row)(col) = Cell(isMine = false)
    }

  }
}
