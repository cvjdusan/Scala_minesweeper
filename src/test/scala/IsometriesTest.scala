package test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import operations._
import model.GameCell

class IsometriesTest extends AnyFunSuite with Matchers {

  def createTestGrid: Vector[Vector[GameCell]] = {
    Vector(
      Vector(GameCell(true), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(true)),
      Vector(GameCell(false), GameCell(true), GameCell(false))
    )
  }

  test("Rotation - clockwise 90 degrees") {
    val rotation = new Rotation(clockwise = true)
    val grid = createTestGrid

    val result = rotation.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(0)(2).isMine shouldBe true
  }

  test("Rotation - counter-clockwise 90 degrees") {
    val rotation = new Rotation(clockwise = false)
    val grid = createTestGrid

    val result = rotation.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(2)(0).isMine shouldBe true
  }

  test("Reflection - horizontal") {
    val reflection = new Reflection("horizontal", None)
    val grid = createTestGrid

    val result = reflection.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(2)(0).isMine shouldBe true
  }

  test("Reflection - vertical") {
    val reflection = new Reflection("vertical", None)
    val grid = createTestGrid

    val result = reflection.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(0)(2).isMine shouldBe true
  }

  test("Translation - move by (1, 1)") {
    val translation = new Translation(1, 1)
    val grid = createTestGrid

    val result = translation.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(1)(1).isMine shouldBe true
  }

  test("CentralSymmetry - 180 degree rotation") {
    val centralSymmetry = new CentralSymmetry()
    val grid = createTestGrid

    val result = centralSymmetry.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(2)(2).isMine shouldBe true
  }

  test("Composition - Rotation >>> Reflection") {
    val rotation = new Rotation(clockwise = true)
    val reflection = new Reflection("horizontal", None)
    val composed = rotation >>> reflection

    val grid = createTestGrid
    val result = composed.apply(grid)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result should not equal grid
  }

  test("IdentityIsometry - no change") {
    val identity = IdentityIsometry
    val grid = createTestGrid

    val result = identity.apply(grid)

    result should equal(grid)

    for {
      row <- grid.indices
      col <- grid.head.indices
    } {
      result(row)(col).isMine shouldBe grid(row)(col).isMine
    }

    result should be theSameInstanceAs grid
  }

  test("Sector-based transformation") {
    val rotation = new Rotation(clockwise = true)
    val sector = Sector(0, 0, 1, 1) // 2x2
    val pivot = (1, 1) // centar
    val grid = Vector(
      Vector(GameCell(true), GameCell(true), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(true)),
      Vector(GameCell(false), GameCell(true), GameCell(false))
    )

    val result = rotation.applyToSector(grid, sector, pivot)

    result.length shouldBe 3
    result.head.length shouldBe 3

    // (0,0) rotira u (0,2)
    result(0)(2).isMine shouldBe true

    // a (0,0) should be cleaned
    result(0)(0).isMine shouldBe false
  }

  test("ExpandingIsometry trait - grid expansion") {
    val expandingRotation = new Rotation(clockwise = true) with ExpandingIsometry
    val sector = Sector(0, 0, 1, 1) // 2x2 sektor
    val pivot = (0, 0) // pivot na (0,0)
    val grid = Vector(
      Vector(GameCell(true), GameCell(false)),
      Vector(GameCell(false), GameCell(false))
    )

    val result = expandingRotation.applyToSector(grid, sector, pivot)

    result.length shouldBe 2
    result.head.length shouldBe 3

    // (0,0) rotirala na (0,1)
    result(0)(1).isMine shouldBe true
  }

  test("TransparentIsometry trait - transparent overlay") {
    val transparentRotation = new Rotation(clockwise = true) with TransparentIsometry
    val sector = Sector(0, 0, 2, 2) // 2x2 sektor
    val pivot = (1, 1) // pivot na centru
    val grid = Vector(
      Vector(GameCell(true), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(true)),
      Vector(GameCell(false), GameCell(true), GameCell(false))
    )

    val result = transparentRotation.applyToSector(grid, sector, pivot)

    result.length shouldBe 3
    result.head.length shouldBe 3

    result(0)(0).isMine shouldBe true // originalna mina ostaje
    result(0)(2).isMine shouldBe true // rotirana mina se dodaje
  }

  test("ExpandingIsometry + TransparentIsometry - combined behavior") {
    val combinedRotation = new Rotation(clockwise = true) with ExpandingIsometry with TransparentIsometry
    val sector = Sector(0, 0, 1, 1) // 2x2 sektor
    val pivot = (0, 0) // pivot na (0,0)
    val grid = Vector(
      Vector(GameCell(true), GameCell(false)),
      Vector(GameCell(false), GameCell(false))
    )

    val result = combinedRotation.applyToSector(grid, sector, pivot)

    // Grid treba da se proširi
    result.length shouldBe 2
    result.head.length shouldBe 3

    println(result)

    // Mina iz (0,0) na (0,1)
    result(0)(1).isMine shouldBe true

    // Izvorni sektor (0,0) je prazno
    result(0)(0).isMine shouldBe false
  }

  test("Translation (non-expanding), out of bands") {
    val t = new Translation(-1, 0)
    val grid = Vector(
      Vector(GameCell(true), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(0, 0, 0, 0) // samo (0,0)
    val pivot = (0, 0)

    val res = t.applyToSector(grid, sector, pivot)

    res.length shouldBe 3
    res.head.length shouldBe 3

    // out of bounds
    res(0)(0).isMine shouldBe false
  }

  test("Transparent vs Opaque (preserve vs overwrite)") {
    // cilj (1,1) ima minu izvor (1,0) je prazan, pa
    // Transparent treba da sacuva minu u cilju, Opaque da je brise
    val grid = Vector(
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(true), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(1, 0, 1, 0)
    val pivot = (0, 0)

    val tTrans = new Translation(1, 0) with TransparentIsometry // dx=+1, dy=0 → (1,0)->(1,1)
    val resT = tTrans.applyToSector(grid, sector, pivot)
    resT(1)(1).isMine shouldBe true // OR čuva postojeću minu

    val tOp = new Translation(1, 0) // Opaque
    val resO = tOp.applyToSector(grid, sector, pivot)
    resO(1)(1).isMine shouldBe false // prepis praznim izvorom
  }
  //
  test("Reflection vertical - transperancy") {

    val refl = new Reflection("vertical", Some(1)) with TransparentIsometry
    val grid = Vector(
      Vector(GameCell(true), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(0, 0, 2, 2)
    val pivot = (1, 1) // irrelevantno za 'vertical', ali ok

    val res = refl.applyToSector(grid, sector, pivot)

    res.length shouldBe 3
    res.head.length shouldBe 3

    // cilj (0,2) new mine
    res(0)(2).isMine shouldBe true
    // izvor (0,0) stays
    res(0)(0).isMine shouldBe true
  }

  test("Reflection vertical - swap (Opaque)") {
    val refl = new Reflection("vertical", Some(1))
    val grid = Vector(
      Vector(GameCell(true), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(0, 0, 2, 2)
    val pivot = (1, 1)

    val res = refl.applyToSector(grid, sector, pivot)

    res.length shouldBe 3
    res.head.length shouldBe 3

    res(0)(2).isMine shouldBe true
    res(0)(0).isMine shouldBe false
  }


  test("Reflection verticala around k=0 – out of bounds") {
    val refl = new Reflection("vertical", Some(0))
    val grid = Vector(
      Vector(GameCell(false), GameCell(true), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(0, 1, 0, 1) // samo (0,1)
    val pivot = (0, 0)

    val res = refl.applyToSector(grid, sector, pivot)

    res.length shouldBe 3
    res.head.length shouldBe 3
    res(0)(1).isMine shouldBe false // Out of bounds
  }
  //
  test("Reflection diagonal-main - transparent") {
    // glavna dijagonala oko pivota (1,1) u 3x3: (0,2) -> (2,0)
    val refl = new Reflection("diagonal-main", None) with TransparentIsometry
    val grid = Vector(
      Vector(GameCell(false), GameCell(false), GameCell(true)),
      Vector(GameCell(false), GameCell(false), GameCell(false)),
      Vector(GameCell(false), GameCell(false), GameCell(false))
    )
    val sector = Sector(0, 0, 2, 2)
    val pivot = (1, 1)

    val res = refl.applyToSector(grid, sector, pivot)

    res.length shouldBe 3
    res.head.length shouldBe 3

    res(2)(0).isMine shouldBe true
    res(0)(2).isMine shouldBe true
  }

  test("LevelValidator – valid/invalid") {
    // valid
    val ok = Vector.fill(8, 8)(GameCell(false))
    LevelValidator.validateLevel(ok, "Beginner").isRight shouldBe true

    val tooManyMines = Vector.fill(8, 8)(GameCell(true))
    LevelValidator.validateLevel(tooManyMines, "Beginner").isLeft shouldBe true

    // dimenzije > limit
    val tooBig = Vector.fill(9, 9)(GameCell(false))
    LevelValidator.validateLevel(tooBig, "Beginner").isLeft shouldBe true

  }

  // inverse

  test("Rotation inverse") {
    val t = new Rotation()
    val inv = t.inverse
    val grid = createTestGrid

    val fwd = t.apply(grid)
    val back = inv.apply(fwd)

    back should equal(grid)
  }

  test("clearOriginalNotInImage should clear mines not mapped into image") {
    val rotation = new Rotation(clockwise = true) with TransparentIsometry
    val sector = Sector(0, 0, 1, 1) // 2x2
    val pivot = (1, 1) // pivot u centru

    val grid = Vector(
      Vector(GameCell(isMine = true),  GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false), GameCell(isMine = false)),
      Vector(GameCell(isMine = false), GameCell(isMine = false), GameCell(isMine = false))
    )

    val result = rotation.applyToSector(grid, sector, pivot)

    // Mina na (0,0) se rotira u (0,2), sto je van sektora (0..1,0..1).
    result(0)(0).isMine shouldBe false
    // a slika se upisuje u (0,2), pa tamo treba da se pojavi mina
    result(0)(2).isMine shouldBe true
  }



}