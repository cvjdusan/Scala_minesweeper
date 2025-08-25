package operations


object IsometryComposer {

  def compose(isometries: Isometry*): Isometry = {
    validateComposition(isometries)

    if (isometries.length == 1) {
      isometries.head
    } else {
      isometries.reduceLeft((first, second) => new ComposedIsometry(first, second))
    }
  }

  private def validateComposition(isometries: Seq[Isometry]): Unit = {
    require(isometries.nonEmpty, "Cannot compose empty sequence of isometries")

    isometries.zipWithIndex.foreach { case (isometry, index) =>
      validateIsometry(isometry, s"isometry[$index]")
    }
  }

  private def validateIsometry(isometry: Isometry, name: String): Unit = {
    require(isometry != null, s"$name must not be null")
    
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
        //
      case c: CentralSymmetry =>
        //
      case c: ComposedIsometry =>
        validateComposedIsometry(c, name)
      case _ =>
        require(isometry != null, s"$name must not be null")
    }
  }

  private def validateComposedIsometry(composed: ComposedIsometry, name: String): Unit = {
    validateIsometry(composed.first, s"$name.first")
    validateIsometry(composed.second, s"$name.second")
  }

}