package operations

import model.GameCell

trait Isometry {
  def apply[A](matrix: Vector[Vector[A]]): Vector[Vector[A]]

  def isExpanding: Boolean = false
  def isTransparent: Boolean = false
  def inverse: Isometry

  def >>>(other: Isometry): Isometry = new ComposedIsometry(this, other)

  def applyToSector[A](grid: Vector[Vector[A]], sector: Sector, pivot: (Int, Int)): Vector[Vector[A]] = {
    // 1. calculate mapped coord
    val mappedCoordinates = calculateMappedCoordinates(sector, pivot)
    println(s"applyToSector: isExpanding=$isExpanding, mappedCoordinates=$mappedCoordinates")

    // 2. if expanding mood
    val (workingGrid, offsetRow, offsetCol) = if (isExpanding) {
      println(s"Expanding grid...")
      val result = expandGridForMappedCoordinates(grid, mappedCoordinates)
      println(s"Expanded grid size: ${result._1.length}x${if (result._1.nonEmpty) result._1.head.length else 0}, offset=(${result._2}, ${result._3})")
      result
    } else {
      println(s"Not expanding grid")
      (grid, 0, 0)
    }

    // 3. Direktno mapiraj koordinate iz sektora na ciljne pozicije
    var result = workingGrid

    // Prvo očisti originalni sektor ako nije transparent
    if (!isTransparent) {
      result = clearOriginalSector(result, sector, offsetRow, offsetCol)
    }

    // Zatim aplikuj mapiranje
    for ((srcRow, srcCol, tgtRow, tgtCol) <- mappedCoordinates) {
      val adjustedTgtRow = tgtRow + offsetRow
      val adjustedTgtCol = tgtCol + offsetCol

      println(s"Mapping: ($srcRow,$srcCol) -> ($tgtRow,$tgtCol) -> adjusted ($adjustedTgtRow,$adjustedTgtCol)")

      // Proverava da li su source koordinate validne
      if (srcRow >= 0 && srcRow < grid.length && srcCol >= 0 && srcCol < grid(0).length) {
        val sourceCell = grid(srcRow)(srcCol)
        println(s"  Source cell at ($srcRow,$srcCol): $sourceCell")

        // Proverava da li su target koordinate u granicama
        if (adjustedTgtRow >= 0 && adjustedTgtRow < result.length &&
            adjustedTgtCol >= 0 && adjustedTgtCol < result(0).length) {

          if (isTransparent) {
            // Transparent overlay - OR logika za mine
            val targetCell = result(adjustedTgtRow)(adjustedTgtCol)
            val finalCell = combineTransparent(sourceCell, targetCell)
            println(s"  Transparent overlay: $sourceCell + $targetCell = $finalCell")
            result = result.updated(adjustedTgtRow, result(adjustedTgtRow).updated(adjustedTgtCol, finalCell))
          } else {
            // Opaque overlay - prepisi target
            println(s"  Opaque overlay: $sourceCell -> ($adjustedTgtRow,$adjustedTgtCol)")
            result = result.updated(adjustedTgtRow, result(adjustedTgtRow).updated(adjustedTgtCol, sourceCell))
          }
        } else if (!isExpanding) {
          // Non-expanding mode - drop OOB koordinate (ne radi ništa)
          println(s"  OOB coordinate dropped: ($adjustedTgtRow,$adjustedTgtCol)")
        }
      } else {
        println(s"  Invalid source coordinates: ($srcRow,$srcCol)")
      }
    }

    result
  }

  // Svaka izometrija mora implementirati ovo
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)]

  // Omogućavam ComposedIsometry da pristupi ovoj metodi
  protected[operations] def calculateMappedCoordinatesForComposition(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    calculateMappedCoordinates(sector, pivot)
  }

  private def clearOriginalSector[A](grid: Vector[Vector[A]], sector: Sector, offsetRow: Int = 0, offsetCol: Int = 0): Vector[Vector[A]] = {
    grid.zipWithIndex.map { case (row, rowIdx) =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        // Prilagodi koordinate za offset
        val adjustedRow = rowIdx - offsetRow
        val adjustedCol = colIdx - offsetCol

        if (sector.contains(adjustedRow, adjustedCol)) {
          // clear mine
          if (cell.isInstanceOf[GameCell]) {
            cell.asInstanceOf[GameCell].copy(isMine = false).asInstanceOf[A]
          } else {
            cell
          }
        } else {
          cell
        }
      }
    }
  }

  private def combineTransparent[A](sourceCell: A, targetCell: A): A = {
    (sourceCell, targetCell) match {
      case (src: GameCell, tgt: GameCell) =>
        // OR logika za mine
        val combinedMine = src.isMine || tgt.isMine
        tgt.copy(isMine = combinedMine).asInstanceOf[A]
      case (src: GameCell, _) => src.asInstanceOf[A]
      case (_, tgt: GameCell) => tgt.asInstanceOf[A]
      case _ => sourceCell.asInstanceOf[A]
    }
  }

  private def expandGridForMappedCoordinates[A](grid: Vector[Vector[A]], mappedCoordinates: Seq[(Int, Int, Int, Int)]): (Vector[Vector[A]], Int, Int) = {
    if (mappedCoordinates.isEmpty) return (grid, 0, 0)

    val currentRows = grid.length
    val currentCols = if (currentRows == 0) 0 else grid.head.length

    // Nađi min/max koordinate u mapiranim ciljevima
    val targetRows = mappedCoordinates.map(_._3)
    val targetCols = mappedCoordinates.map(_._4)

    val minTargetRow = targetRows.min
    val maxTargetRow = targetRows.max
    val minTargetCol = targetCols.min
    val maxTargetCol = targetCols.max

    // Izračunaj offset za negativne koordinate
    val offsetRow = math.max(0, -minTargetRow)
    val offsetCol = math.max(0, -minTargetCol)

    // Izračunaj potrebne dimenzije uključujući offset
    val totalRows = math.max(currentRows + offsetRow, maxTargetRow + offsetRow + 1)
    val totalCols = math.max(currentCols + offsetCol, maxTargetCol + offsetCol + 1)

    // Proširi grid - koristi praznu GameCell ili prvi element iz grid-a
    val empty = if (currentRows > 0 && currentCols > 0) {
      grid(0)(0) match {
        case _: GameCell => GameCell(false).asInstanceOf[A]
        case other => other
      }
    } else {
      GameCell(false).asInstanceOf[A]
    }
    val expandedGrid = Vector.tabulate(totalRows) { row =>
      Vector.tabulate(totalCols) { col =>
        // Originalni grid se pomera za offset
        val originalRow = row - offsetRow
        val originalCol = col - offsetCol
        if (originalRow >= 0 && originalRow < currentRows &&
            originalCol >= 0 && originalCol < currentCols) {
          grid(originalRow)(originalCol)
        } else {
          empty
        }
      }
    }

    (expandedGrid, offsetRow, offsetCol)
  }


}

trait TransparentIsometry extends Isometry {
  override def isTransparent: Boolean = true
}

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

  // Operator za primenu više puta
  def applyNTimes(n: Int): Isometry = {
    if (n <= 0) IdentityIsometry
    else if (n == 1) this
    else {
      val half = applyNTimes(n / 2)
      if (n % 2 == 0) half >>> half else half >>> half >>> this
    }
  }
}

case object IdentityIsometry extends Isometry {
  override def isExpanding: Boolean = false
  override def isTransparent: Boolean = true

  override def apply[A](grid: Vector[Vector[A]]): Vector[Vector[A]] = grid
  override def inverse: Isometry = this

  // Identity transformacija - sve koordinate ostaju iste
  protected def calculateMappedCoordinates(sector: Sector, pivot: (Int, Int)): Seq[(Int, Int, Int, Int)] = {
    for {
      srcRow <- sector.topLeftRow to sector.bottomRightRow
      srcCol <- sector.topLeftCol to sector.bottomRightCol
    } yield {
      (srcRow, srcCol, srcRow, srcCol) // source = target
    }
  }
}
