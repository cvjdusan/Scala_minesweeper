package operations
import model.GameCell

case class CentralSymmetry() extends Isometry {
  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    Rotation(clockwise = true)(Rotation(clockwise = true)(grid)) // two times rotation
  }
}
