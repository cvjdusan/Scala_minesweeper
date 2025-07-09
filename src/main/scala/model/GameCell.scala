package model

final case class GameCell(
                           isMine: Boolean,
                           isRevealed: Boolean = false,
                           adjacent: Int = 0,
                           isFlagged: Boolean = false
                         )
