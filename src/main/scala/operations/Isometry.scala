package operations

import model.GameCell

trait Isometry {
  def apply[A](matrix: Vector[Vector[A]]): Vector[Vector[A]]
}
