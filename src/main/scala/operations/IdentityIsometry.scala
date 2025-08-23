package operations

import model.GameCell

case object IdentityIsometry extends Isometry {
  override def isExpanding: Boolean = false

  override def isTransparent: Boolean = true

  override def apply(grid: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = grid

  override def inverse: Isometry = this

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      (srcRow, srcCol, srcRow, srcCol) // source = target
    }
  }
}
