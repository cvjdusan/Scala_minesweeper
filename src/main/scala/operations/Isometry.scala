package operations
import model.GameCell

trait Isometry {
  def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]]
}
