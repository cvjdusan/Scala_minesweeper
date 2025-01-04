class GameController(gridSize: Int, numMines: Int) {

  private val grid: Array[Array[Cell]] = Array.ofDim[Cell](gridSize, gridSize)


  def initializeGame(): Unit = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      grid(row)(col) = Cell(isMine = false)
    }

    placeMines()
    calculateAdjacentMines()
  }

  private def placeMines(): Unit = {
    val random = scala.util.Random
    var minesPlaced = 0
    while(minesPlaced < numMines) {
      val row = random.nextInt(gridSize)
      val col = random.nextInt(gridSize)
      if (!grid(row)(col).isMine) {
        grid(row)(col).isMine = true
        minesPlaced += 1
      }
    }
  }

  private def calculateAdjacentMines(): Unit = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      if (!grid(row)(col).isMine) {
        grid(row)(col).adjacentMines = countAdjacentMines(row, col)
      }
    }
  }

  private def countAdjacentMines(i: Int, j: Int): Int = {
    val cellsToCheck = Seq(
      (i-1, j-1), (i-1, j), (i-1, j+1),
      (i, j-1), (i, j+1),
      (i+1, j-1), (i+1, j), (i+1, j+1)
    )

    cellsToCheck
      .filter{ case(ii, jj) => ii >= 0 && ii < gridSize && jj >= 0 && jj < gridSize}
      .count { case(ii, jj) => checkIsMine(ii, jj)}
  }

  def checkIsMine(row: Int, col: Int) : Boolean = {
    grid(row)(col).isMine
  }

  def getMineCount(row: Int, col: Int) : Int = {
    grid(row)(col).adjacentMines
  }




}
