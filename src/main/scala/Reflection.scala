case class Reflection(axis: String) extends Isometry {

  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    axis.toLowerCase match {
      case "horizontal" => grid.reverse // Horizontalna refleksija (preko reda)
      case "vertical"   => grid.map(_.reverse) // Vertikalna refleksija (preko kolone)
      case "diagonal-main" => grid.transpose
      case "diagonal-secondary" => grid.reverse.transpose.map(_.reverse)
      case _            => throw new IllegalArgumentException("Invalid axis")
    }
  }

}
