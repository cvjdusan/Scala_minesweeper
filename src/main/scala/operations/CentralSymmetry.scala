package operations

import model.GameCell

case class CentralSymmetry() extends Isometry {

  override def apply(g: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    val rows = g.length
    val cols = if (rows == 0) 0 else g.head.length
    if (rows == 0 || cols == 0) return g
    
    val sector = Sector(0, 0, rows - 1, cols - 1)
    val pivot = (rows / 2, cols / 2)

    (Rotation() >>> Rotation()).applyToSector(g, sector, pivot)
  }

  override def inverse: Isometry = this

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    (Rotation() >>> Rotation()).calculateMappedCoordinatesForComposition(sector, pivot)
  }
}
