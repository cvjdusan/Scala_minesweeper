package operations

import model.GameCell
import operations.Sector

trait Isometry {
  def apply(matrix: Vector[Vector[GameCell]]): Vector[Vector[GameCell]]

  def isExpanding: Boolean = false

  def isTransparent: Boolean = false

  def inverse: Isometry

  def >>>(other: Isometry): Isometry = new ComposedIsometry(this, other)

  def applyToSector(grid: Vector[Vector[GameCell]], sector: Sector, pivot: (Int, Int)): Vector[Vector[GameCell]] = {
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


    result = clearOriginalNotInImage(result, sector, mappedCoordinates, offsetRow, offsetCol)

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

        if (adjustedTgtRow >= 0 && adjustedTgtRow < result.length && adjustedTgtCol >= 0 && adjustedTgtCol < result(0).length) {

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
          // out of bounds
          println(s"  OOB coordinate dropped: ($adjustedTgtRow,$adjustedTgtCol)")
        }
      } else {
        println(s"  Invalid source coordinates: ($srcRow,$srcCol)")
      }
    }

    result
  }

  // Default
  protected def expandGrid(grid: Vector[Vector[GameCell]], mappedCoordinates: Seq[(Int, Int, Int, Int)]): (Vector[Vector[GameCell]], Int, Int) = {
    (grid, 0, 0)
  }

  protected def combineCells(sourceCell: GameCell, targetCell: GameCell): GameCell =
    sourceCell

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)]

  private def clearOriginalSector(grid: Vector[Vector[GameCell]],
                                  sector: Sector,
                                  offsetRow: Int = 0,
                                  offsetCol: Int = 0): Vector[Vector[GameCell]] = {
    grid.zipWithIndex.map { case (row, rowIdx) =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        val adjustedRow = rowIdx - offsetRow
        val adjustedCol = colIdx - offsetCol
        if (sector.contains(adjustedRow, adjustedCol)) cell.copy(isMine = false) else cell
      }
    }
  }

  protected[operations] def calculateMappedCoordinatesForComposition(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    calculateMappedCoordinates(sector, pivot)
  }

  private def clearOriginalNotInImage(
                                       grid: Vector[Vector[GameCell]],
                                       sector: Sector,
                                       mapped: Seq[(Int, Int, Int, Int)],
                                       offsetRow: Int,
                                       offsetCol: Int
                                     ): Vector[Vector[GameCell]] = {
    grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        val rr = r - offsetRow
        val cc = c - offsetCol

        val inOriginal =
          rr >= sector.topLeftRow && rr <= sector.bottomRightRow &&
            cc >= sector.topLeftCol && cc <= sector.bottomRightCol

        val inImage = mapped.exists { case (_, _, tr, tc) => tr == rr && tc == cc }

        if (inOriginal && !inImage) cell.copy(isMine = false) else cell
      }
    }
  }


}
