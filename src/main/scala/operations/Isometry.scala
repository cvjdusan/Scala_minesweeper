package operations

import model.GameCell
import operations.Sector

trait Isometry {
  def apply[A](matrix: Vector[Vector[A]]): Vector[Vector[A]]

  def isExpanding: Boolean = false

  def isTransparent: Boolean = false

  def inverse: Isometry

  def >>>(other: Isometry): Isometry = new ComposedIsometry(this, other)

  def applyToSector[A](grid: Vector[Vector[A]], sector: Sector, pivot: (Int, Int)): Vector[Vector[A]] = {

    val mappedCoordinates = calculateMappedCoordinates(sector, pivot)
    println(s"applyToSector: isExpanding=$isExpanding, mappedCoordinates=$mappedCoordinates")

    val (workingGrid, offsetRow, offsetCol) = if (isExpanding) {
      println(s"Expanding grid...")
      expandGrid(grid, mappedCoordinates)
    } else {
      println(s"Not expanding grid")
      (grid, 0, 0)
    }


    var result = workingGrid

    if (!isTransparent) {
      result = clearOriginalSector(result, sector, offsetRow, offsetCol)
    }

    // apply mapping
    for ((srcRow, srcCol, tgtRow, tgtCol) <- mappedCoordinates) {
      val adjustedTgtRow = tgtRow + offsetRow
      val adjustedTgtCol = tgtCol + offsetCol

      println(s"Mapping: ($srcRow,$srcCol) -> ($tgtRow,$tgtCol) -> adjusted ($adjustedTgtRow,$adjustedTgtCol)")

      if (srcRow >= 0 && srcRow < grid.length && srcCol >= 0 && srcCol < grid(0).length) {
        val sourceCell = grid(srcRow)(srcCol)
        println(s"  Source cell at ($srcRow,$srcCol): $sourceCell")

        if (adjustedTgtRow >= 0 && adjustedTgtRow < result.length &&
          adjustedTgtCol >= 0 && adjustedTgtCol < result(0).length) {

          if (isTransparent) {
            // Transparent overlay OR logika za mine
            val targetCell = result(adjustedTgtRow)(adjustedTgtCol)
            val finalCell = combineCells(sourceCell, targetCell)
            println(s"  Transparent overlay: $sourceCell + $targetCell = $finalCell")
            result = result.updated(adjustedTgtRow, result(adjustedTgtRow).updated(adjustedTgtCol, finalCell))
          } else {
            println(s"  Opaque overlay: $sourceCell -> ($adjustedTgtRow,$adjustedTgtCol)")
            result = result.updated(adjustedTgtRow, result(adjustedTgtRow).updated(adjustedTgtCol, sourceCell))
          }
        } else if (!isExpanding) {
          // Non-expanding mode - drop OOB koordinate (ne radi ništa)
          println(s"  OOB coordinate dropped: ($adjustedTgtRow,$adjustedTgtCol)")
        }
      } else {
        println(s"  Invalid source coordinates: ($srcRow,$srcCol)")
      }
    }

    result
  }

  // Default
  protected def expandGrid[A](grid: Vector[Vector[A]], mappedCoordinates: Seq[(Int, Int, Int, Int)]): (Vector[Vector[A]], Int, Int) = {
    (grid, 0, 0)
  }

  protected def combineCells[A](sourceCell: A, targetCell: A): A = {
    // Default
    sourceCell
  }

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)]

  private def clearOriginalSector[A](grid: Vector[Vector[A]], sector: Sector, offsetRow: Int = 0, offsetCol: Int = 0): Vector[Vector[A]] = {
    grid.zipWithIndex.map { case (row, rowIdx) =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        val adjustedRow = rowIdx - offsetRow
        val adjustedCol = colIdx - offsetCol

        if (sector.contains(adjustedRow, adjustedCol)) {
          // clear mine
          if (cell.isInstanceOf[GameCell]) {
            cell.asInstanceOf[GameCell].copy(isMine = false).asInstanceOf[A]
          } else {
            cell
          }
        } else {
          cell
        }
      }
    }
  }

  def applyNTimes(n: Int): Isometry = {
    if (n <= 0) IdentityIsometry
    else if (n == 1) this
    else {
      val half = applyNTimes(n / 2)
      if (n % 2 == 0) half >>> half else half >>> half >>> this
    }
  }


  protected[operations] def calculateMappedCoordinatesForComposition(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    calculateMappedCoordinates(sector, pivot)
  }

}
