package operations

import model.GameCell

case class CompositeIsometry(isometries: Seq[Isometry]) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = isometries.foldLeft(g)((cur, iso) => iso(cur))
}