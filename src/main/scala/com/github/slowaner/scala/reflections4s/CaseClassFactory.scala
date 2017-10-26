package com.github.slowaner.scala.reflections4s

import scala.reflect.runtime.{universe => ru}

case class CaseClassFactory[T](ttag: ru.TypeTag[T]) extends ClassFactory[T] {

  val mirror: ru.Mirror = ttag.mirror
  val tpe: ru.Type = ttag.tpe
  val classSymbol: ru.ClassSymbol = tpe.typeSymbol.asClass
  val moduleSymbol: ru.ModuleSymbol = classSymbol.companion.asModule
  val instanceMirror: ru.InstanceMirror = mirror reflect (mirror reflectModule moduleSymbol).instance
  val instanceMirrorType: ru.Type = instanceMirror.symbol.typeSignature

  if (!classSymbol.isCaseClass) throw new IllegalArgumentException(s"$classSymbol is not a case class. Use CommonCaseClassFactoryInstead")

  val applyMethodSymbol: ru.MethodSymbol = (instanceMirrorType member ru.TermName("apply")).asMethod

  val applyMethodParams: List[ru.Symbol] = applyMethodSymbol.paramLists.flatten

  val defaultApplyMethod: ru.MethodMirror = instanceMirror reflectMethod applyMethodSymbol

  val defaultApplyMethodDefaultParamValues: Map[String, ru.MethodMirror] =
    applyMethodParams.zipWithIndex.map { case (symbol, index) =>
      val name = symbol.name.toString
      val defarg = instanceMirrorType member ru.TermName(s"apply$$default$$${index + 1}")
      val reflectedMethod = if (defarg != ru.NoSymbol) {
        instanceMirror reflectMethod defarg.asMethod
      } else null
      name -> reflectedMethod
    }.filterNot(_._2 == null).toMap

  override protected def buildWith(args: Seq[_]): T = defaultApplyMethod(args: _*).asInstanceOf[T]

  override def buildWith(bindings: Map[String, Any]): T = {
    if (applyMethodParams.exists(tp => !(bindings.contains(tp.name.toString)
      || defaultApplyMethodDefaultParamValues.contains(tp.name.toString)
      || tp.typeSignature <:< ru.typeOf[Option[Any]]))) throw new Exception("Not enough actual parameters")
    val builtArgs = applyMethodParams.map {
      case term if term.typeSignature <:< ru.typeOf[Option[Any]] =>
        val termName = term.name.toString
        if (bindings.contains(termName)) {
          bindings.get(termName).map {
            case vl: Option[Any] => ReflectionHelper.castValue(vl, term.typeSignature)
            case vl => ReflectionHelper.castValue(vl, term.typeSignature.typeArgs.head)
          }
        } else defaultApplyMethodDefaultParamValues(termName)()
      case term =>
        val termName = term.name.toString
        if (bindings.contains(termName)) {
          ReflectionHelper.castValue(bindings(termName), term.typeSignature)
        } else defaultApplyMethodDefaultParamValues(termName)()
    }

    buildWith(builtArgs)
  }
}
