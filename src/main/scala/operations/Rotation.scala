package operations

import model.GameCell

case class Rotation(clockwise: Boolean = true, expanding: Boolean = false) extends Isometry {
  override def isExpanding: Boolean = expanding

  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length // first head length
    if (rows == 0 || cols == 0) return g // no rotation
    
    val sector = Sector(0, 0, rows - 1, cols - 1)
    val pivot = (rows / 2, cols / 2)

    applyToSector(g, sector, pivot)
  }

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
