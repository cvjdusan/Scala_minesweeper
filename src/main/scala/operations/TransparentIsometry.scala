package operations

import model.GameCell

trait TransparentIsometry extends Isometry {
  override def isTransparent: Boolean = true

  // Override-ujemo combineCells metodu iz Isometry trait-a
  override protected def combineCells[A](sourceCell: A, targetCell: A): A = {
    (sourceCell, targetCell) match {
      case (src: GameCell, tgt: GameCell) =>
        // OR logika za mine
        val combinedMine = src.isMine || tgt.isMine
        tgt.copy(isMine = combinedMine).asInstanceOf[A]
      case (src: GameCell, _) => src.asInstanceOf[A]
      case (_, tgt: GameCell) => tgt.asInstanceOf[A]
      case _ => sourceCell.asInstanceOf[A]
    }
  }
}