case class InverseIsometry(isometry: Isometry) extends Isometry {
  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = isometry match {
    case Rotation(clockwise) =>
      Rotation(!clockwise).apply(grid) // Inverz rotacije: suprotan smer
    case Reflection(axis) =>
      Reflection(axis).apply(grid) // Refleksija je simetrična: njen inverz je sama refleksija
    case _ =>
      throw new UnsupportedOperationException("No inverse defined for this isometry")
  }
}
