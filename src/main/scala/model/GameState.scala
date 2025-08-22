import model.GameCell

import java.time.Instant

case class GameState
(
  grid: Vector[Vector[GameCell]],
  startTime: Instant,
  clickCount: Int,
  levelCompleted: Boolean,
  score: Long,
  suggested: Set[(Int, Int)]

) {
  def withGrid(newGrid: Vector[Vector[GameCell]]): GameState = copy(grid = newGrid)

  def withStartTime(time: Instant): GameState = copy(startTime = time)

  def incrementClickCount: GameState = copy(clickCount = clickCount + 1)

  def withLevelCompleted: GameState = copy(levelCompleted = true)

  def withScore(newScore: Long): GameState = copy(score = newScore)

  def decreaseScore(amount: Int): GameState = copy(score = math.max(0, score - amount))

  def addSuggested(move: (Int, Int)): GameState = copy(suggested = suggested + move)

  def rowsLength: Int = grid.length

  def colsLength: Int = if (grid.isEmpty) 0 else grid.head.length

  def isGameLost: Boolean =
    grid.exists(_.exists(c => c.isRevealed && c.isMine))
}

object GameState {
  def empty: GameState = GameState(
    grid = Vector.empty,
    startTime = Instant.EPOCH,
    clickCount = 0,
    levelCompleted = false,
    score = 1000,
    suggested = Set.empty
  )

} 