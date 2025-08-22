package operations

import model.GameCell
import operations.Sector
import operations.Isometry
import operations.TransparentIsometry

object TranslationDebug {
  var callCount = 0
}

case class Translation(dx: Int, dy: Int) extends Isometry {

  override def apply[A](g: Vector[Vector[A]]): Vector[Vector[A]] = {
    TranslationDebug.callCount += 1

    println(s"Input grid size: ${g.length}x${if (g.nonEmpty) g.head.length else 0}")

    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g

    val sector = Sector(0, 0, rows - 1, cols - 1)
    val pivot = (0, 0) // ignore pivot for translation

    println(s"Translation.apply calling applyToSector...")
    val result = applyToSector(g, sector, pivot)
    println(s"Translation.apply finished, result grid size: ${result.length}x${if (result.nonEmpty) result.head.length else 0}")
    result
  }

  override def inverse: Isometry = Translation(-dx, -dy)

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
