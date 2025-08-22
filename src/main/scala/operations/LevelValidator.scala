package operations

import model.GameCell

trait LevelValidator {
  def validateLevel(grid: Vector[Vector[GameCell]], difficulty: String): Either[String, Boolean]
}

object LevelValidator extends LevelValidator {
  private val difficultyLimits: Map[String, (Int, Int, Int)] = Map(
    "Beginner" -> (8, 8, 10),    // max 8x8, max 10 mina
    "Normal" -> (16, 16, 40),    // max 16x16, max 40 mina
    "Advanced" -> (30, 16, 99)   // max 30x16, max 99 mina
  )

  override def validateLevel(grid: Vector[Vector[GameCell]], difficulty: String): Either[String, Boolean] = {
    val (maxRows, maxCols, maxMines) = difficultyLimits.getOrElse(difficulty, (0, 0, 0))

    val dimensionValidation = validateDimensions(grid, maxRows, maxCols)
    val mineCountValidation = validateMineCount(grid, maxMines)

    for {
      _ <- dimensionValidation
      _ <- mineCountValidation
    } yield true
  }

  private def validateDimensions(grid: Vector[Vector[GameCell]], maxRows: Int, maxCols: Int): Either[String, Unit] = {
    if (grid.isEmpty) {
      Left("Grid cannot be empty")
    } else if (grid.length > maxRows || grid.head.length > maxCols) {
      Left(s"Grid dimensions ${grid.length}x${grid.head.length} exceed limits ${maxRows}x${maxCols}")
    } else {
      Right(())
    }
  }
  
  private def validateMineCount(grid: Vector[Vector[GameCell]], maxMines: Int): Either[String, Unit] = {
    val mineCount = grid.flatten.count(_.isMine)
    if (mineCount > maxMines) {
      Left(s"Mine count $mineCount exceeds limit $maxMines")
    } else {
      Right(())
    }
  }

} 