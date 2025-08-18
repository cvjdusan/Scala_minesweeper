import model.GameCell
import java.nio.file.{Files, Path}
import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._

object GameIO {

  sealed trait GameError

  case class FileError(message: String) extends GameError

  case class ParseError(message: String) extends GameError

  case class ValidationError(message: String) extends GameError

  def saveGame(state: GameState, path: Path): Either[GameError, Unit] = {
    Try {
      val lines = state.grid.map { row =>
        row.map { cell =>
          if (cell.isMine && cell.isRevealed) "X"
          else if (cell.isMine) "#"
          else if (cell.isFlagged) "F"
          else if (cell.isRevealed) "R"
          else "-"
        }.mkString
      }
      Files.write(path, lines.asJava)
      ()
    }.toEither.left.map(error => FileError(s"Failed to save game: ${error.getMessage}"))
  }

  def loadGame(lines: Array[String]): Either[GameError, Vector[Vector[GameCell]]] = {
    // Remove leading and trailing blank lines
    val trimmed =
      lines.dropWhile(_.trim.isEmpty).reverse.dropWhile(_.trim.isEmpty).reverse

    if (trimmed.isEmpty)
      return Left(ValidationError("No lines provided"))

    // Disallow empty lines inside the grid
    if (trimmed.exists(_.trim.isEmpty))
      return Left(ValidationError("Empty line inside grid"))

    // All rows must have the same length
    val widths = trimmed.map(_.length)
    if (widths.distinct.size != 1 || widths.head == 0)
      return Left(ValidationError("Inconsistent row lengths"))

    import scala.util.Try
    Try {
      trimmed.map { line =>
        line.map {
          case '#' => GameCell(isMine = true)
          case '-' => GameCell(isMine = false)
          case 'R' => GameCell(isMine = false, isRevealed = true)
          case 'F' => GameCell(isMine = false, isFlagged = true)
          case ch => throw new IllegalArgumentException(s"Invalid character: $ch")
        }.toVector
      }.toVector
    }.toEither.left.map(e => ParseError(s"Failed to parse game data: ${e.getMessage}"))
  }

  def validateGrid(grid: Vector[Vector[GameCell]]): Either[GameError, Unit] = {
    if (grid.isEmpty) {
      Left(ValidationError("Grid cannot be empty"))
    } else if (grid.exists(_.isEmpty)) {
      Left(ValidationError("Grid rows cannot be empty"))
    } else if (grid.map(_.length).distinct.length != 1) {
      Left(ValidationError("All grid rows must have the same length"))
    } else {
      Right(())
    }
  }

} 