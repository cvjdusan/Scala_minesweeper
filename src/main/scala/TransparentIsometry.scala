case class TransparentIsometry(isometry: Isometry) extends Isometry {
  override def apply(grid: Array[Array[GameCell]]): Array[Array[GameCell]] = {
    val transformed = isometry(grid)
    grid.zip(transformed).map { case (originalRow, transformedRow) =>
      originalRow.zip(transformedRow).map { case (originalCell, transformedCell) =>
        if (transformedCell.isMine) transformedCell else originalCell
      }
    }
  }
}
