package com.github.slowaner.scala.reflections

import com.github.slowaner.scala.reflections4s.ClassFactory
import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.runtime.{universe => ru}

case class MockCaseClass(
  mockString: String,
  mockIntDefault: Int = 10,
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

  private val mockCaseFactory = ClassFactory[MockCaseClass](ru.typeTag[MockCaseClass])
  private val mockCommonFactory = ClassFactory[MockCommonClass](ru.typeTag[MockCommonClass])

  "Reflections4s CaseClassFactory" should s"build $mockCaseWithDefault from $bindingsWithDefault" in {
    mockCaseFactory.buildWith(bindingsWithDefault) should be(mockCaseWithDefault)
  }

  it should s"build $mockCaseAll from $bindingsAll" in {
    mockCaseFactory.buildWith(bindingsAll) should be(mockCaseAll)
  }

  "Reflections4s CaseCommonFactory" should s"build $mockCommonWithDefault from $bindingsWithDefault" in {
    mockCommonFactory.buildWith(bindingsWithDefault) should be(mockCommonWithDefault)
  }

  it should s"build $mockCommonAll from $bindingsAll" in {
    mockCommonFactory.buildWith(bindingsAll) should be(mockCommonAll)
  }
}
