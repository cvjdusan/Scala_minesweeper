class GameController() {

  private var grid: Array[Array[Cell]] = Array.ofDim[Cell](0, 0)

  private val EMPTY_SPACE = '-'
  private val MINE_SPACE = '#'

  def initGrid(rows: Int, columns: Int) : Unit = {
    grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns) {
      grid(row)(col) = Cell(isMine = false)
    }
  }

  def initializeGame(rows: Int, columns: Int): Unit = {
    initGrid(rows, columns)

    //placeMines(numMines)
    calculateAdjacentMines(rows, columns)
  }

//  private def placeMines(numMines: Int): Unit = {
//    val random = scala.util.Random
//    var minesPlaced = 0
//    while(minesPlaced < numMines) {
//      val row = random.nextInt(gridSize)
//      val col = random.nextInt(gridSize)
//      if (!grid(row)(col).isMine) {
//        grid(row)(col).isMine = true
//        minesPlaced += 1
//      }
//    }
//  }

  private def calculateAdjacentMines(rows: Int, columns: Int): Unit = {
    for (row <- 0 until rows; col <- 0 until columns) {
      if (!grid(row)(col).isMine) {
        grid(row)(col).adjacentMines = countAdjacentMines(row, col, rows, columns)
      }
    }
  }

  private def countAdjacentMines(i: Int, j: Int, rows: Int, columns: Int): Int = {
    val cellsToCheck = Seq(
      (i-1, j-1), (i-1, j), (i-1, j+1),
      (i, j-1), (i, j+1),
      (i+1, j-1), (i+1, j), (i+1, j+1)
    )

    cellsToCheck
      .filter{ case(ii, jj) => ii >= 0 && ii < rows && jj >= 0 && jj < columns}
      .count { case(ii, jj) => checkIsMine(ii, jj)}
  }

  def checkIsMine(row: Int, col: Int) : Boolean = {
    grid(row)(col).isMine
  }

  def getMineCount(row: Int, col: Int) : Int = {
    grid(row)(col).adjacentMines
  }


  def loadGame(gridData: Array[String]): Unit = {
    val gridDimensions = (gridData.length, gridData(0).length)

    initGrid(gridDimensions._1, gridDimensions._2)

    gridData.zipWithIndex.foreach { case (line, row) =>
      line.zipWithIndex.foreach { case (char, col) =>
        grid(row)(col).isMine = char == MINE_SPACE
      }
    }

    calculateAdjacentMines(gridDimensions._1, gridDimensions._2)
  }


}
