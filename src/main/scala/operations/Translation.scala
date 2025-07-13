package operations

import model.GameCell

case class Translation(dx: Int, dy: Int) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    val newRows = rows + math.abs(dy)
    val newCols = cols + math.abs(dx)
    val empty = g.headOption.flatMap(_.headOption).getOrElse(().asInstanceOf[A])
    var res = Vector.fill(newRows, newCols)(empty)

    for {
      r <- g.indices
      c <- g(r).indices
      nr = r + dy
      nc = c + dx
      if nr >= 0 && nr < newRows && nc >= 0 && nc < newCols
    } res = res.updated(nr, res(nr).updated(nc, g(r)(c)))

    res
  }
}
