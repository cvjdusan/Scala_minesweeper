package operations

import model.GameCell

trait TransparentIsometry extends Isometry {
  override def isTransparent: Boolean = true

  override protected def combineCells(sourceCell: GameCell, targetCell: GameCell): GameCell = {
    (sourceCell, targetCell) match {
      case (src: GameCell, tgt: GameCell) =>
        val combinedMine = src.isMine || tgt.isMine
        tgt.copy(isMine = combinedMine)
      case (src: GameCell, _) => src
      case (_, tgt: GameCell) => tgt
      case _ => sourceCell
    }
  }
}