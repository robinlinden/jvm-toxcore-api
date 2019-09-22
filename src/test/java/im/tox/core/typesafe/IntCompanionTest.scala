package im.tox.core.typesafe

import im.tox.core.ModuleCompanionTest
import org.scalacheck.{ Arbitrary, Gen }

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
abstract class IntCompanionTest[T <: AnyVal](module: IntCompanion[T]) extends ModuleCompanionTest(module) {

  protected def genValidInt: Gen[Int]
  protected def genInvalidInt: Gen[Int]

  test("fromInt (valid)") {
    forAll(genValidInt) { int: Int =>
      assert(module.fromInt(int).isDefined)
    }
  }

  test("fromInt (invalid)") {
    forAll(genInvalidInt) { int: Int =>
      assert(module.fromInt(int).isEmpty)
    }
  }

  test("toInt") {
    forAll(genValidInt) { int: Int =>
      assert(module.toInt(module.fromInt(int).get) == int)
    }
  }

}
