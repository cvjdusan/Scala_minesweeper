package model

final case class GameCell(
                           isMine: Boolean,
                           isRevealed: Boolean = false,
                           adjacentMines: Int = 0,
                           isFlagged: Boolean = false
                         )
