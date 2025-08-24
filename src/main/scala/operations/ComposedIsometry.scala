package operations

import model.GameCell


case class ComposedIsometry(first: Isometry, second: Isometry) extends Isometry {
  override def isExpanding: Boolean = first.isExpanding || second.isExpanding

  override def isTransparent: Boolean = first.isTransparent && second.isTransparent

  override def apply(grid: Vector[Vector[GameCell]]): Vector[Vector[GameCell]] = {
    require(grid.nonEmpty && grid.head.nonEmpty, "Grid must not be empty")
    require(grid.length > 0, "Grid must have at least one row")
    require(grid.head.length > 0, "Grid must have at least one column")

    validateIsometry(first, "first")
    validateIsometry(second, "second")

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
        Sector(firstTgtRow, firstTgtRow, firstTgtRow, firstTgtCol),
        pivot
      )
      secondMapped.map { case (_, _, secondTgtRow, secondTgtCol) =>
        (srcRow, srcCol, secondTgtRow, secondTgtCol)
      }
    }
  }

  private def validateIsometry(isometry: Isometry, name: String): Unit = {
    require(isometry != null, s"$name isometry must not be null")
    
    isometry match {
      case t: Translation => 
        require(t.dx >= -100 && t.dx <= 100, s"$name Translation dx must be between -1000 and 1000")
        require(t.dy >= -100 && t.dy <= 100, s"$name Translation dy must be between -1000 and 1000")
      case r: Reflection =>
        require(r.axis.nonEmpty, s"$name Reflection axis must not be empty")
        require(Set("horizontal", "vertical", "diagonal-main", "diagonal-secondary").contains(r.axis.toLowerCase), 
                s"Invalid $name reflection axis: ${r.axis}. Must be one of: horizontal, vertical, diagonal-main, diagonal-secondary")
        r.axisPosition.foreach { pos =>
          require(pos >= -100 && pos <= 100, s"$name reflection axis position must be between -1000 and 1000")
        }
      case r: Rotation =>
        // ?
      case c: CentralSymmetry =>
        // ?
      case c: ComposedIsometry =>
        validateComposedIsometry(c, name)
      case _ =>
        require(isometry != null, s"$name isometry must not be null")
    }
  }

  private def validateComposedIsometry(composed: ComposedIsometry, name: String): Unit = {
    validateIsometry(composed.first, s"$name.first")
    validateIsometry(composed.second, s"$name.second")
  }
}