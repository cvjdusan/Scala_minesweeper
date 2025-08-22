package model

final case class GameCell(
                           isMine: Boolean,
                           isRevealed: Boolean = false,
                           adjacentMines: Int = 0,
                           isFlagged: Boolean = false
                         )

object GameCellOps {
  implicit class GameCellExtensions(val cell: GameCell) { // extends AnyVal {
    def when(condition: Boolean)(f: GameCell => GameCell): GameCell = {
      if (condition) f(cell) else cell
    }

    def unless(condition: Boolean)(f: GameCell => GameCell): GameCell = {
      when(!condition)(f)
    }
  }
}
