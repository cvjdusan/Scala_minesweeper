package operations


case class ComposedIsometry(first: Isometry, second: Isometry) extends Isometry {
  override def isExpanding: Boolean = first.isExpanding || second.isExpanding
  override def isTransparent: Boolean = first.isTransparent && second.isTransparent

  override def apply[A](grid: Vector[Vector[A]]): Vector[Vector[A]] = {
    require(grid.nonEmpty && grid.head.nonEmpty, "Grid must not be empty")
    second.apply(first.apply(grid))
  }

  override def inverse: Isometry = new ComposedIsometry(second.inverse, first.inverse)

  // Kompozicija sektorskih transformacija
  override def applyToSector[A](grid: Vector[Vector[A]], sector: Sector, pivot: (Int, Int)): Vector[Vector[A]] = {
    // Prvo primeni prvu izometriju na sektor
    val intermediateGrid = first.applyToSector(grid, sector, pivot)

    // Zatim primeni drugu izometriju na rezultat
    // Za kompoziciju, koristimo isti sektor i pivot
    second.applyToSector(intermediateGrid, sector, pivot)
  }

  // Za kompoziciju, mapirane koordinate su kompozicija mapiranja
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    val firstMapped = first.calculateMappedCoordinatesForComposition(sector, pivot)

    // Aplikuj drugu transformaciju na mapirane koordinate
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