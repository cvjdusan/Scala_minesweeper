package operations
import model.GameCell

case class Reflection(axis: String, axisPosition: Option[Int] = None, expanding: Boolean = false) extends Isometry {
  override def isExpanding: Boolean = expanding
  override def isTransparent: Boolean = false
  
  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    if (g.isEmpty || g.head.isEmpty) g
    else {
      val (rows, cols) = (g.length, g.head.length)
      
      axis.toLowerCase match {
        case "horizontal" =>
          val pos = axisPosition.getOrElse(rows / 2)
          if (expanding) {
            // Proširujem grid da sve stane
            val newRows = math.max(rows, 2 * pos + 1)
            val expandedGrid = Vector.fill(newRows)(Vector.fill(cols)(g(0)(0)))
            // Kopiram originalni grid
            var tempGrid = expandedGrid
            for (i <- 0 until rows; j <- 0 until cols) {
              tempGrid = tempGrid.updated(i, tempGrid(i).updated(j, g(i)(j)))
            }
            val finalExpandedGrid = tempGrid
            // Reflektujem oko pozicije pos
            reflectHorizontalAroundPosition(finalExpandedGrid, pos)
          } else {
            // Ne proširujem - koristim postojeći grid
            reflectHorizontalAroundPosition(g, pos)
          }
          
        case "vertical" =>
          val pos = axisPosition.getOrElse(cols / 2)
          if (expanding) {
            // Proširujem grid da sve stane
            val newCols = math.max(cols, 2 * pos + 1)
            val expandedGrid = Vector.fill(rows)(Vector.fill(newCols)(g(0)(0)))
            // Kopiram originalni grid
            var tempGrid = expandedGrid
            for (i <- 0 until rows; j <- 0 until cols) {
              tempGrid = tempGrid.updated(i, tempGrid(i).updated(j, g(i)(j)))
            }
            val finalExpandedGrid = tempGrid
            // Reflektujem oko pozicije pos
            reflectVerticalAroundPosition(finalExpandedGrid, pos)
          } else {
            // Ne proširujem - koristim postojeći grid
            reflectVerticalAroundPosition(g, pos)
          }
          
        case "diagonal-main" =>
          if (expanding) {
            // Proširujem grid da sve stane
            val maxDim = math.max(rows, cols)
            val expandedGrid = Vector.fill(maxDim)(Vector.fill(maxDim)(g(0)(0)))
            // Kopiram originalni grid
            var tempGrid = expandedGrid
            for (i <- 0 until rows; j <- 0 until cols) {
              tempGrid = tempGrid.updated(i, tempGrid(i).updated(j, g(i)(j)))
            }
            val finalExpandedGrid = tempGrid
            finalExpandedGrid.transpose
          } else {
            g.transpose
          }
          
        case "diagonal-secondary" =>
          if (expanding) {
            // Proširujem grid da sve stane
            val maxDim = math.max(rows, cols)
            val expandedGrid = Vector.fill(maxDim)(Vector.fill(maxDim)(g(0)(0)))
            // Kopiram originalni grid
            var tempGrid = expandedGrid
            for (i <- 0 until rows; j <- 0 until cols) {
              tempGrid = tempGrid.updated(i, tempGrid(i).updated(j, g(i)(j)))
            }
            val finalExpandedGrid = tempGrid
            finalExpandedGrid.transpose.map(_.reverse).reverse
          } else {
            g.transpose.map(_.reverse).reverse
          }
          
        case _ => throw new IllegalArgumentException("Invalid axis")
      }
    }
  }
  
  private def reflectHorizontalAroundPosition[A](grid: Vector[Vector[A]], pos: Int): Vector[Vector[A]] = {
    val rows = grid.length
    grid.zipWithIndex.map { case (row, rowIdx) =>
      val distanceFromAxis = rowIdx - pos
      val reflectedRow = pos - distanceFromAxis
      if (reflectedRow >= 0 && reflectedRow < rows) {
        grid(reflectedRow)
      } else {
        row // Ako je reflektovana pozicija van granica, ostavljam original
      }
    }
  }
  
  private def reflectVerticalAroundPosition[A](grid: Vector[Vector[A]], pos: Int): Vector[Vector[A]] = {
    val cols = grid.head.length
    grid.map { row =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        val distanceFromAxis = colIdx - pos
        val reflectedCol = pos - distanceFromAxis
        if (reflectedCol >= 0 && reflectedCol < cols) {
          row(reflectedCol)
        } else {
          cell // Ako je reflektovana pozicija van granica, ostavljam original
        }
      }
    }
  }

  override def inverse: Isometry = this // refleksija je svoj inverz

  // Implementacija koja mapira samo sektor oko zadate ose
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val (pivotRow, pivotCol) = pivot
    
    // Iteriraj kroz sve ćelije u sektoru
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      val (tgtRow, tgtCol) = axis.toLowerCase match {
        case "horizontal" =>
          val pos = axisPosition.getOrElse(pivotRow)
          // Formula: (r,c) → (2k - r, c)
          (2 * pos - srcRow, srcCol)
          
        case "vertical" =>
          val pos = axisPosition.getOrElse(pivotCol)
          // Formula: (r,c) → (r, 2k - c)
          (srcRow, 2 * pos - srcCol)
          
        case "diagonal-main" =>
          // Formula: (r,c) → (c, r)
          (srcCol, srcRow)
          
        case "diagonal-secondary" =>
          // Formula: (r,c) → (c', r') kao kompozicija glavne + rotacija 180°
          // Prvo refleksija oko glavne dijagonale: (r,c) → (c, r)
          // Zatim rotacija 180°: (c, r) → (-c, -r) + pivot
          val reflectedRow = srcCol
          val reflectedCol = srcRow
          val rotatedRow = pivotRow - (reflectedRow - pivotRow)
          val rotatedCol = pivotCol - (reflectedCol - pivotCol)
          (rotatedRow, rotatedCol)
          
        case _ => (srcRow, srcCol) // nepoznata osa - vraća original
      }
      
      (srcRow, srcCol, tgtRow, tgtCol)
    }
  }
}
