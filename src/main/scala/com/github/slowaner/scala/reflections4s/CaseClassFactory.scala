package com.github.slowaner.scala.reflections4s

import scala.language.postfixOps
import scala.reflect.runtime.{universe => ru}

case class CaseClassFactory[T](ttag: ru.TypeTag[T]) extends ClassFactory[T] {

  if (!isCaseClass) throw new IllegalArgumentException(s"$classSymbol is not a case class. Use CommonCaseClassFactoryInstead")

  val applyMethodSymbol: ru.MethodSymbol = moduleInstanceMirrorType member ru.TermName("apply") asMethod

  val applyMethodParams: List[ru.Symbol] = applyMethodSymbol.paramLists flatten

  val defaultApplyMethod: ru.MethodMirror = moduleInstanceMirror reflectMethod applyMethodSymbol

  val defaultApplyMethodDefaultParamValues: Map[String, ru.MethodMirror] =
    applyMethodParams.zipWithIndex map { case (symbol, index) =>
      val name = symbol.name toString
      val defarg = moduleInstanceMirrorType member ru.TermName(s"apply$$default$$${index + 1}")
      val reflectedMethod = if (defarg != ru.NoSymbol) {
        moduleInstanceMirror reflectMethod defarg.asMethod
      } else null
      name -> reflectedMethod
    } filterNot (_._2 == null) toMap

  override protected def buildWith(args: Seq[_]): T = defaultApplyMethod(args: _*).asInstanceOf[T]

  override def buildWith(bindings: Map[String, Any]): T = {
    if (applyMethodParams exists (tp => !(bindings.contains(tp.name toString)
      || defaultApplyMethodDefaultParamValues.contains(tp.name toString)
      || tp.typeSignature <:< ru.typeOf[Option[Any]]))) throw new Exception("Not enough actual parameters")
    val builtArgs = applyMethodParams map {
      case term if term.typeSignature <:< ru.typeOf[Option[Any]] =>
        val termName = term.name toString
        val result = if (bindings contains termName) ReflectionHelper castValue(bindings(termName), term.typeSignature)
        else if (defaultApplyMethodDefaultParamValues contains termName) defaultApplyMethodDefaultParamValues(termName)()
        else None
        result
      case term =>
        val termName = term.name toString
        val result = if (bindings.contains(termName)) ReflectionHelper castValue(bindings(termName), term.typeSignature)
        else defaultApplyMethodDefaultParamValues(termName)()
        result
    }

    buildWith(builtArgs)
  }
}
