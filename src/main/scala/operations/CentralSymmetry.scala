package operations

case class CentralSymmetry() extends Isometry {
  // isExpanding se postavlja preko ExpandingIsometry trait-a
  
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g
    
    val sector = Sector(0, 0, rows - 1, cols - 1)
    val pivot = (rows / 2, cols / 2)

    applyToSector(g, sector, pivot)
  }

  override def inverse: Isometry = this

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val (pivotRow, pivotCol) = pivot

    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      val tgtRow = 2 * pivotRow - srcRow
      val tgtCol = 2 * pivotCol - srcCol
      
      (srcRow, srcCol, tgtRow, tgtCol)
    }
  }
}
