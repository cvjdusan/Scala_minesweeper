package operations

import model.GameCell

case class InverseIsometry(iso: Isometry) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = iso match {
    case Rotation(cw) => Rotation(!cw)(g)
    case r: Reflection => r(g) // same as original
    case _ => throw new UnsupportedOperationException("No inverse defined for this isometry")
  }
}

