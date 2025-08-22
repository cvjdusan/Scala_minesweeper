package operations


object IsometryComposer {

  def compose(isometries: Isometry*): Isometry = {
    validateComposition(isometries)

    if (isometries.isEmpty) {
      IdentityIsometry
    } else if (isometries.length == 1) {
      isometries.head
    } else {
      isometries.reduceLeft((first, second) => new ComposedIsometry(first, second))
    }
  }

  private def validateComposition(isometries: Seq[Isometry]): Unit = {
    require(isometries.nonEmpty, "Cannot compose empty sequence of isometries")
    require(isometries.forall(_ != null), "Cannot compose null isometries")

    isometries.foreach { iso =>
      require(iso.isInstanceOf[Isometry], s"Invalid isometry type: ${iso.getClass}")
    }
  }

}