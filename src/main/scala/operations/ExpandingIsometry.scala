package operations

import model.GameCell

case class ExpandingIsometry(iso: Isometry) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val t = iso(g)
    val maxRows = math.max(g.length, t.length)
    val maxCols = math.max(g.headOption.map(_.length).getOrElse(0), t.headOption.map(_.length).getOrElse(0))

    def expand[B](m: Vector[Vector[B]], rows: Int, cols: Int, empty: => B): Vector[Vector[B]] = {
      val filledCols = m.map(row => row ++ Vector.fill(cols - row.length)(empty))
      filledCols ++ Vector.fill(rows - filledCols.length)(Vector.fill(cols)(empty))
    }

    val emptyCell = g.headOption.flatMap(_.headOption).getOrElse(().asInstanceOf[A])
    val eg = expand(g, maxRows, maxCols, emptyCell)
    val et = expand(t, maxRows, maxCols, emptyCell)
    eg.zip(et).map { case (r1, r2) => r1.zip(r2).map(_._2) }
  }
}