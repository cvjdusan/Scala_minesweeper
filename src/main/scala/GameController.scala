import scala.annotation.tailrec

class GameController() {

  private var grid: Array[Array[GameCell]] = Array.ofDim[GameCell](0, 0)



  private val EMPTY_SPACE = '-'
  private val MINE_SPACE = '#'

  def initGrid(rows: Int, columns: Int) : Unit = {
    grid = Array.ofDim[GameCell](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns) {
      grid(row)(col) = GameCell(isMine = false)
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

  def getCell(row: Int, col: Int) : GameCell = {
    grid(row)(col)
  }

  def getGrid() : Array[Array[GameCell]] = {
    grid
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

  def revealCell(row: Int, col: Int) : Unit = {
    if(row < 0 || row >= grid.length || col < 0 || col >= grid(row).length)
      return

    val cell = grid(row)(col)

    if(cell.isRevealed || cell.isMine)
      return

    cell.isRevealed = true

    if(cell.adjacentMines > 0)
      return

    val neighbors = getNeighbors(row, col)
    neighbors.foreach { case (r, c) => revealCell(r, c) }

  }

  private def getNeighbors(row: Int, col: Int): Seq[(Int, Int)] = {
    Seq(
      (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
      (row, col - 1),                     (row, col + 1),
      (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
    ).filter {
      case (r, c) => r >= 0 && r < grid.length && c >= 0 && c < grid(0).length
    }
  }

  def revealAllMines(): Unit = {
    grid.foreach(row => row.foreach(cell => if (cell.isMine) cell.isRevealed = true))
  }

}
