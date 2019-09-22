package im.tox.core.typesafe

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

abstract class DiscreteIntCompanionTest[T <: AnyVal](module: DiscreteIntCompanion[T]) extends IntCompanionTest(module) {

  override protected final def genValidInt: Gen[Int] = Gen.oneOf(module.values)
  override protected final def genInvalidInt: Gen[Int] = arbitrary[Int].filter(!module.values.contains(_))

}
