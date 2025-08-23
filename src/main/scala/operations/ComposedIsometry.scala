package operations

import model.GameCell


case class ComposedIsometry(first: Isometry, second: Isometry) extends Isometry {
  override def isExpanding: Boolean = first.isExpanding || second.isExpanding

  override def isTransparent: Boolean = first.isTransparent && second.isTransparent

  override def apply(grid: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    require(grid.nonEmpty && grid.head.nonEmpty, "Grid must not be empty")
    second.apply(first.apply(grid))
  }

  override def inverse: Isometry = new ComposedIsometry(second.inverse, first.inverse)


  override def applyToSector(grid: Vector[Vector[GameCell]], sector: Sector, pivot: (Int, Int)): Vector[Vector[GameCell]] = {
    val intermediateGrid = first.applyToSector(grid, sector, pivot)
    second.applyToSector(intermediateGrid, sector, pivot)
  }

  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val firstMapped = first.calculateMappedCoordinatesForComposition(sector, pivot)

    firstMapped.flatMap { case (srcRow, srcCol, firstTgtRow, firstTgtCol) =>
      val secondMapped = second.calculateMappedCoordinatesForComposition(
        Sector(firstTgtRow, firstTgtCol, firstTgtRow, firstTgtCol),
        pivot
      )
      secondMapped.map { case (_, _, secondTgtRow, secondTgtCol) =>
        (srcRow, srcCol, secondTgtRow, secondTgtCol)
      }
    }
  }
}