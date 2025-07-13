package operations

import model.GameCell

case class TransparentIsometry(iso: Isometry) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val t = iso(g)
    val maxRows = math.max(g.length, t.length)
    val maxCols = math.max(g.head.length, t.head.length)

    def cellOr[B](r: Int, c: Int, m: Vector[Vector[B]], fallback: => B): B =
      if (r < m.length && c < m(r).length) m(r)(c) else fallback

    val empty = g.headOption.flatMap(_.headOption).getOrElse(().asInstanceOf[A])
    Vector.tabulate(maxRows, maxCols) { (r, c) =>
      val tc = cellOr(r, c, t, empty)
      val oc = cellOr(r, c, g, empty)
      tc match {
        case gc: GameCell if gc.isMine => tc
        case _ => oc
      }
    }
  }
}