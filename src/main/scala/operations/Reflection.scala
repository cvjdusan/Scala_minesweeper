package operations
import model.GameCell

case class Reflection(axis: String) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = axis.toLowerCase match {
    case "horizontal"         => g.reverse
    case "vertical"           => g.map(_.reverse)
    case "diagonal-main"      => g.transpose
    case "diagonal-secondary" => g.reverse.transpose.map(_.reverse)
    case _                     => throw new IllegalArgumentException("Invalid axis")
  }
}
