package com.github.slowaner.scala.reflections

import com.github.slowaner.scala.reflections4s.ClassFactory
import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.runtime.{universe => ru}

case class MockCaseClass(
  mockString: String,
  mockIntDefault: Int = 10,
  mockInt: Int
)

class Reflections4sTest extends FlatSpec with Matchers {

  private val bindingsWithDefault = Map(
    "mockString" -> "JustString",
    "mockInt" -> 55
  )
  private val mockWithDefault = MockCaseClass(
    mockString = "JustString",
    mockInt = 55
  )
  private val bindingsAll = Map(
    "mockIntDefault" -> 333,
    "mockString" -> "JustString",
    "mockInt" -> 55
  )
  private val mockAll = MockCaseClass(
    mockIntDefault = 333,
    mockString = "JustString",
    mockInt = 55
  )

  private val factory = new ClassFactory[MockCaseClass](ru.typeTag[MockCaseClass])

  "Reflections4s CaseClassFactory" should s"build $mockWithDefault from $bindingsWithDefault" in {
    factory.buildWith(bindingsWithDefault) should be(mockWithDefault)
  }

  it should s"build $mockAll from $bindingsAll" in {
    factory.buildWith(bindingsAll) should be(mockAll)
  }
}
