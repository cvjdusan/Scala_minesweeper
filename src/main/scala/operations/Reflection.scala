package operations

import model.GameCell
import operations.Sector

case class Reflection(axis: String, axisPosition: Option[Int] = None) extends Isometry {


  override def apply(g: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g

    val sector = Sector(0, 0, rows - 1, cols - 1)
    val pivot = axis.toLowerCase match {
      case "horizontal" => (axisPosition.getOrElse(rows / 2), 0)
      case "vertical" => (0, axisPosition.getOrElse(cols / 2))
      case "diagonal-main" | "diagonal-secondary" => (rows / 2, cols / 2)
      case _ => (rows / 2, cols / 2)
    }

    applyToSector(g, sector, pivot)
  }

  override def inverse: Isometry = this // refleksija je svoj inverz

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val (pivotRow, pivotCol) = pivot

    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      val (tgtRow, tgtCol) = axis.toLowerCase match {
        case "horizontal" =>
          val pos = axisPosition.getOrElse(pivotRow)
          // (r,c) → (2k - r, c)
          (2 * pos - srcRow, srcCol)

        case "vertical" =>
          val pos = axisPosition.getOrElse(pivotCol)
          // (r,c) → (r, 2k - c)
          (srcRow, 2 * pos - srcCol)

        case "diagonal-main" =>
          // (r,c) → (c, r)
          (srcCol, srcRow)

        case "diagonal-secondary" =>
          // (r,c) → (c', r') kao kompozicija glavne + rotacija 180°
          // Prvo refleksija oko glavne dijagonale: (r,c) → (c, r)
          // Zatim rotacija 180°: (c, r) → (-c, -r) + pivot
          val reflectedRow = srcCol
          val reflectedCol = srcRow
          val rotatedRow = pivotRow - (reflectedRow - pivotRow)
          val rotatedCol = pivotCol - (reflectedCol - pivotCol)
          (rotatedRow, rotatedCol)

        case _ => (srcRow, srcCol) // unknown axis, return original
      }

      (srcRow, srcCol, tgtRow, tgtCol)
    }
  }
}
