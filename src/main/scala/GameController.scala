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
    var cnt = 0

    if(i - 1 >= 0) {
      if(j - 1 >= 0) {
        if(openCell(i-1, j-1)) cnt = cnt + 1
      }
      if(openCell(i-1, j)) cnt = cnt.+(1)
      if(j + 1 < gridSize) {
        if(openCell(i-1, j+1)) cnt = cnt + 1
      }
    }

    if(j - 1 >= 0) {
      if(openCell(i, j-1)) cnt = cnt + 1
    }

    if(j + 1 < gridSize) {
      if(openCell(i, j+1)) cnt = cnt + 1
    }


    if(i + 1 < gridSize) {
      if(j - 1 >= 0) {
        if(openCell(i+1, j-1)) cnt = cnt + 1
      }
      if(openCell(i+1, j)) cnt = cnt.+(1)
      if(j + 1 < gridSize) {
        if(openCell(i+1, j+1)) cnt = cnt + 1
      }
    }


    cnt
  }

  def openCell(row: Int, col: Int) : Boolean = {
    val cell = grid(row)(col)

    cell.isMine
  }

  def getMineCount(row: Int, col: Int) : Int = {
    val cell = grid(row)(col)

    cell.adjacentMines
  }


}
