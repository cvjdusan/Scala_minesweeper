package operations

case class CentralSymmetry(expanding: Boolean = false) extends Isometry {
  override def isExpanding: Boolean = expanding
  override def isTransparent: Boolean = false
  
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    if (expanding) {
      // Proširujem grid da sve stane
      val (rows, cols) = (g.length, g.head.length)
      val maxDim = math.max(rows, cols)
      val expandedGrid = Vector.fill(maxDim)(Vector.fill(maxDim)(g(0)(0)))
      
      // Kopiram originalni grid
      var tempGrid = expandedGrid
      for (i <- 0 until rows; j <- 0 until cols) {
        tempGrid = tempGrid.updated(i, tempGrid(i).updated(j, g(i)(j)))
      }
      val finalExpandedGrid = tempGrid

      Rotation(clockwise = true, expanding)(Rotation(clockwise = true, expanding)(finalExpandedGrid))
    } else {
      Rotation(clockwise = true, expanding)(Rotation(clockwise = true, expanding)(g))
    }
  }

  override def inverse: Isometry = this // its own inverse
  
  // Implementacija koja mapira samo sektor oko pivot tačke
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val (pivotRow, pivotCol) = pivot
    
    // Iteriraj kroz sve ćelije u sektoru
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      // Centralna simetrija: (r,c) → (2*pivotRow - r, 2*pivotCol - c)
      val tgtRow = 2 * pivotRow - srcRow
      val tgtCol = 2 * pivotCol - srcCol
      
      (srcRow, srcCol, tgtRow, tgtCol)
    }
  }
}
