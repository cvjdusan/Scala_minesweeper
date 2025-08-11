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
    Try {
      lines.map { line =>
        line.map {
          case '#' => GameCell(isMine = true)
          case '-' => GameCell(isMine = false)
          case 'R' => GameCell(isMine = false, isRevealed = true)
          case 'F' => GameCell(isMine = false, isFlagged = true)
          case char => throw new IllegalArgumentException(s"Invalid character: $char")
        }.toVector
      }.toVector
    }.toEither.left.map(error => ParseError(s"Failed to parse game data: ${error.getMessage}"))
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

  def readGameFile(path: Path): Either[GameError, Array[String]] = {
    Try {
      Files.readAllLines(path).asScala.toArray
    }.toEither.left.map(error => FileError(s"Failed to read file: ${error.getMessage}"))
  }

  def loadGameFromFile(path: Path): Either[GameError, Vector[Vector[GameCell]]] = {
    for {
      lines <- readGameFile(path)
      grid <- loadGame(lines)
      _ <- validateGrid(grid)
    } yield grid
  }
} 