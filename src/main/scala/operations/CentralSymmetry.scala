package operations

import model.GameCell

case object CentralSymmetry extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = Rotation(clockwise = true)(Rotation(clockwise = true)(g))
}
