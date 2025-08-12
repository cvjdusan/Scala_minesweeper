
object Game {

  private var state: GameState = GameState.empty

  def get: GameState = state
  def set(s: GameState): Unit = state = s

  def update(f: GameState => GameState): GameState = {
    val next = f(state)
    if (next ne state) state = next
    state
  }
}