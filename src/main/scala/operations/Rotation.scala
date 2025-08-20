package operations

import model.GameCell

case class Rotation(clockwise: Boolean = true, expanding: Boolean = false) extends Isometry {
  override def isExpanding: Boolean = expanding
  override def isTransparent: Boolean = false

  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] =
    if (g.isEmpty || g.head.isEmpty) g
    else if (clockwise) g.transpose.map(_.reverse) // transpose = rows into cols, flip cols
    else g.transpose.reverse // transpose => rows into cols, flip rows

  override def inverse: Isometry = Rotation(!clockwise)

  protected def calculateMappedCoordinates(
                                            sector: Sector,
                                            pivot: (Int, Int)
                                          ): Seq[(Int, Int, Int, Int)] = {

    val (pivotRow, pivotCol) = pivot
    val buffer = scala.collection.mutable.ListBuffer[(Int, Int, Int, Int)]()

    for (srcRow <- sector.topLeftRow to sector.bottomRightRow) {
      for (srcCol <- sector.topLeftCol to sector.bottomRightCol) {

        // relative to pivot
        val relRow = srcRow - pivotRow
        val relCol = srcCol - pivotCol

        // rotation
        val (newRelRow, newRelCol) =
          if (clockwise) {
            // clockwise: (x,y) → (y,-x)
            (relCol, -relRow)
          } else {
            // counter-clockwise: (x,y) → (-y,x)
            (-relCol, relRow)
          }

        // add pivot to move relative result back to original grid
        val tgtRow = pivotRow + newRelRow
        val tgtCol = pivotCol + newRelCol

        buffer += ((srcRow, srcCol, tgtRow, tgtCol))
      }
    }

    buffer.toSeq
  }

}
