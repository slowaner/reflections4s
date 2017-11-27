package com.github.slowaner.scala.reflections

import com.github.slowaner.scala.reflections4s.{ClassFactory, ReflectionHelper}
import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.runtime.{universe => ru}

case class MockCaseClass(
  mockString: String,
  mockIntDefault: Int = 10,
  mockInt: Int
)

case class MockCaseClassNoDefaults(
  mockString: String,
  mockIntDefault: Int,
  mockInt: Int
)

class MockCommonClass(
  val mockDoubleDefault: Double = 10.5d,
  val mockString: String,
  val mockInt: Int
) {
  override def toString = s"MockCommonClass($mockDoubleDefault,$mockString,$mockInt)"

  override def equals(obj: Any): Boolean = obj match {
    case null => false
    case mcc: MockCommonClass =>
      if (
        mcc.mockDoubleDefault != this.mockDoubleDefault ||
          mcc.mockString != this.mockString ||
          mcc.mockInt != this.mockInt
      ) false
      else true
    case _ => false
  }
}

class MockCommonClassNoDefaults(
  val mockDoubleDefault: Double,
  val mockString: String,
  val mockInt: Int
) {
  override def toString = s"MockCommonClassNoDefaults($mockDoubleDefault,$mockString,$mockInt)"

  override def equals(obj: Any): Boolean = obj match {
    case null => false
    case mccnd: MockCommonClassNoDefaults =>
      if (
        mccnd.mockDoubleDefault != this.mockDoubleDefault ||
          mccnd.mockString != this.mockString ||
          mccnd.mockInt != this.mockInt
      ) false
      else true
    case _ => false
  }
}

class Reflections4sTest extends FlatSpec with Matchers {

  private val bindingsWithDefault = Map(
    "mockString" -> "JustString",
    "mockInt" -> 55
  )

  private val bindingsAll = Map(
    "mockIntDefault" -> 333,
    "mockDoubleDefault" -> 987.115d,
    "mockString" -> "JustString",
    "mockInt" -> 55
  )

  private val mockCaseWithDefault = MockCaseClass(
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCaseAll = MockCaseClass(
    mockIntDefault = 333,
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCommonWithDefault = new MockCommonClass(
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCommonAll = new MockCommonClass(
    mockDoubleDefault = 987.115d,
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCaseNoDefault = MockCaseClassNoDefaults(
    mockIntDefault = 333,
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCommonNoDefault = new MockCommonClassNoDefaults(
    mockDoubleDefault = 987.115d,
    mockString = "JustString",
    mockInt = 55
  )

  private val mockCaseFactory = ClassFactory[MockCaseClass](ru.typeTag[MockCaseClass])
  private val mockCaseNoDefaultsFactory = ClassFactory[MockCaseClassNoDefaults](ru.typeTag[MockCaseClassNoDefaults])
  private val mockCommonFactory = ClassFactory[MockCommonClass](ru.typeTag[MockCommonClass])
  private val mockCommonNoDefaultsFactory = ClassFactory[MockCommonClassNoDefaults](ru.typeTag[MockCommonClassNoDefaults])

  "Reflections4s CaseClassFactory" should s"build $mockCaseWithDefault from $bindingsWithDefault" in {
    mockCaseFactory.buildWith(bindingsWithDefault) shouldBe mockCaseWithDefault
  }

  it should s"build $mockCaseAll from $bindingsAll" in {
    mockCaseFactory.buildWith(bindingsAll) shouldBe mockCaseAll
  }

  it should s"build $mockCaseNoDefault from $bindingsAll" in {
    mockCaseNoDefaultsFactory.buildWith(bindingsAll) shouldBe mockCaseNoDefault
  }

  "Reflections4s CommonClassFactory" should s"build $mockCommonWithDefault from $bindingsWithDefault" in {
    mockCommonFactory.buildWith(bindingsWithDefault) shouldBe mockCommonWithDefault
  }

  it should s"build $mockCommonAll from $bindingsAll" in {
    mockCommonFactory.buildWith(bindingsAll) shouldBe mockCommonAll
  }

  it should s"build $mockCommonNoDefault from $bindingsAll" in {
    mockCommonNoDefaultsFactory.buildWith(bindingsAll) shouldBe mockCommonNoDefault
  }

  "ReflectionHelper" should s"build correct Manifest" in {
    val optionTtag = ru.typeTag[Option[String]]
    val optionManifest = implicitly[Manifest[Option[String]]]
    ReflectionHelper.manifestFor(optionTtag) shouldBe optionManifest
  }
}
