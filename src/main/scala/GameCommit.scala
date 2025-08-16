
final class GameCommit(
                        controller: GameController,
                        render: (GameState, Boolean) => Unit,
                        onOver: () => Unit,
                        onWin: () => Unit
                      ) {

  def update(f: (GameController, GameState) => GameState,
             isOver: GameState => Boolean = _ => false): Unit = {
    val next = f(controller, Game.get)
    Game.set(next)
   // render(next, isOver(next))
    if (isOver(next)) onOver()
    else if (controller.isWin(next)) onWin()
  }
}
