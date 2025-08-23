package operations

case object IdentityIsometry extends Isometry {
  override def isExpanding: Boolean = false

  override def isTransparent: Boolean = true

  override def apply[A](grid: Vector[Vector[A]]): Vector[Vector[A]] = grid

  override def inverse: Isometry = this

  // Identity transformacija  sve koordinate ostaju iste
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      (srcRow, srcCol, srcRow, srcCol) // source = target
    }
  }
}
