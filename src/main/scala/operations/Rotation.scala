package operations

import model.GameCell

case class Rotation(clockwise: Boolean = true) extends Isometry {

  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] =
    if (g.isEmpty || g.head.isEmpty) g
    else if (clockwise) g.transpose.map(_.reverse)
    else g.reverse.transpose

}
