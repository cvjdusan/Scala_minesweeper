package test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import operations._
import model.GameCell

class NamedIsometryRegistryTest extends AnyFunSuite with Matchers {

  test("Save and retrieve isometry") {
    val rotation = new Rotation(clockwise = true)
    val name = "test_rotation"
    
    NamedIsometryRegistry.saveIsometry(name, rotation)
    val retrieved = NamedIsometryRegistry.getIsometry(name)
    
    retrieved shouldBe Some(rotation)
  }
  
  test("Get all names") {
    NamedIsometryRegistry.clear()
    
    val rotation = new Rotation(clockwise = true)
    val reflection = new Reflection("horizontal", None)
    
    NamedIsometryRegistry.saveIsometry("rot", rotation)
    NamedIsometryRegistry.saveIsometry("refl", reflection)
    
    val names = NamedIsometryRegistry.getAllNames
    names should contain("rot")
    names should contain("refl")
    names.length shouldBe 2
  }
  
  test("Remove isometry") {
    val rotation = new Rotation(clockwise = true)
    val name = "temp_rotation"
    
    NamedIsometryRegistry.saveIsometry(name, rotation)
    NamedIsometryRegistry.getIsometry(name) shouldBe Some(rotation)
    
    NamedIsometryRegistry.removeIsometry(name) shouldBe true
    NamedIsometryRegistry.getIsometry(name) shouldBe None
  }
  
  test("Clear all isometries") {
    val rotation = new Rotation(clockwise = true)
    val reflection = new Reflection("horizontal", None)
    
    NamedIsometryRegistry.saveIsometry("rot", rotation)
    NamedIsometryRegistry.saveIsometry("refl", reflection)
    
    NamedIsometryRegistry.getAllNames.length shouldBe 2
    
    NamedIsometryRegistry.clear()
    NamedIsometryRegistry.getAllNames shouldBe empty
  }
  
  test("Save composed isometry") {
    val composed = new Rotation(clockwise = true) >>> new Reflection("horizontal", None)
    val name = "composed_iso"
    
    NamedIsometryRegistry.saveIsometry(name, composed)
    val retrieved = NamedIsometryRegistry.getIsometry(name)
    
    retrieved shouldBe Some(composed)
    retrieved.get shouldBe a[ComposedIsometry]
  }
} 