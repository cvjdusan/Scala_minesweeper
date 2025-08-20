package operations

import model.GameCell

object TranslationDebug {
  var callCount = 0
}

case class Translation(dx: Int, dy: Int, expanding: Boolean = true) extends Isometry {
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    TranslationDebug.callCount += 1
    println(s"Translation.apply called #${TranslationDebug.callCount} with dx=$dx, dy=$dy, expanding=$expanding")
    if (expanding) {
      applyExpanding(g)
    } else {
      applyNonExpanding(
        g.asInstanceOf[Vector[Vector[GameCell]]],
        dx,
        dy
      ).asInstanceOf[Vector[Vector[A]]]
    }
  }

  override def isExpanding: Boolean = expanding

  override def isTransparent: Boolean = false

  override def inverse: Isometry = Translation(-dx, -dy, expanding)

  private def applyExpanding[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g

    val newRows = rows + math.abs(dy) // new row count after expansion
    val newCols = cols + math.abs(dx) // new column count after expansion

    val offR = math.max(0, -dy) // vertical offset if dy is negative
    val offC = math.max(0, -dx) // horizontal offset if dx is negative

    // define an "empty" cell to fill new positions
    val empty: A =
      if (g.headOption.flatMap(_.headOption).exists(_.isInstanceOf[GameCell]))
        GameCell(false).asInstanceOf[A] // if grid has GameCell, create an empty one (no mine)
      else
        g.headOption.flatMap(_.headOption) // otherwise, take first available element
          .getOrElse(throw new IllegalArgumentException("Cannot expand empty grid without an empty value"))

    // create new bigger grid filled with empty cells
    var res = Vector.fill(newRows)(Vector.fill(newCols)(empty))

    // copy all original cells into their new shifted positions
    for {
      r <- g.indices
      c <- g(r).indices
    } {
      val nr = r + dy + offR // new row index
      val nc = c + dx + offC // new column index
      res = res.updated(nr, res(nr).updated(nc, g(r)(c))) // place original cell into new grid
    }

    res // return expanded grid
  }


  private def applyNonExpanding(g: Vector[Vector[GameCell]], dx: Int, dy: Int): Vector[Vector[GameCell]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g

    // 1) array will with empty
    val empty = GameCell(isMine = false)
    val arr = Array.fill(rows, cols)(empty)

    // 2) write the original ones to new positions with dx, dy
    var r = 0
    while (r < rows) {
      var c = 0
      while (c < cols) {
        val nr = r + dy
        val nc = c + dx
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
          arr(nr)(nc) = g(r)(c)
        }
        c += 1
      }
      r += 1
    }

    // 3) back to Vector[Vector[GameCell]]
    arr.iterator.map(row => row.toVector).toVector
  }


  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      val tgtRow = srcRow + dy
      val tgtCol = srcCol + dx

      (srcRow, srcCol, tgtRow, tgtCol)
    }
  }
}
