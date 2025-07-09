package operations
import model.GameCell

case class CompositeIsometry(isometries: Seq[Isometry]) extends Isometry {

  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    isometries.foldLeft(grid)((currentGrid, isometry) => isometry(currentGrid))
  }

}
