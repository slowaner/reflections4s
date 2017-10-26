package com.github.slowaner.scala.reflections4s

import scala.reflect.runtime.{universe => ru}

case class CommonClassFactory[T](ttag: ru.TypeTag[T]) extends ClassFactory[T] {

  val mirror: ru.Mirror = ttag.mirror
  val tpe: ru.Type = ttag.tpe
  val classSymbol: ru.ClassSymbol = tpe.typeSymbol.asClass
  val moduleSymbol: ru.ModuleSymbol = classSymbol.companion.asModule
  val instanceMirror: ru.InstanceMirror = mirror reflect (mirror reflectModule moduleSymbol).instance
  val instanceMirrorType: ru.Type = instanceMirror.symbol.typeSignature
  val isCaseClass: Boolean = classSymbol.isCaseClass

  val classMirror: ru.ClassMirror = mirror.reflectClass(classSymbol)

  val constructorSymbol: ru.Symbol = tpe.decl(ru.termNames.CONSTRUCTOR)

  val defaultConstructorSymbol: ru.MethodSymbol =
    if (constructorSymbol.isMethod) constructorSymbol.asMethod
    else constructorSymbol.asTerm.alternatives.map {
      _.asMethod
    }.find {
      _.isPrimaryConstructor
    }.get

  val defaultConstructorParams: List[ru.Symbol] = defaultConstructorSymbol.paramLists.flatten

  val defaultConstructorMethod: ru.MethodMirror = classMirror reflectConstructor defaultConstructorSymbol

  val defaultConstructorDefaultParamValues: Map[String, ru.MethodMirror] = defaultConstructorParams.zipWithIndex.map { case (symbol, index) =>
    val name = symbol.name.toString
    val defarg = instanceMirrorType member ru.TermName(s"$$lessinit$$greater$$default$$${index + 1}")
    val reflectedMethod = if (defarg != ru.NoSymbol) {
      instanceMirror reflectMethod defarg.asMethod
    } else null
    name -> reflectedMethod
  }.filterNot(_._2 == null).toMap

  override protected def buildWith(args: Seq[_]): T = defaultConstructorMethod(args: _*).asInstanceOf[T]

  override def buildWith(bindings: Map[String, Any]): T = {
    if (defaultConstructorParams.exists(tp => !(bindings.contains(tp.name.toString)
      || defaultConstructorDefaultParamValues.contains(tp.name.toString)
      || tp.typeSignature <:< ru.typeOf[Option[Any]]))) throw new Exception("Not enough actual parameters")
    val builtArgs = defaultConstructorParams.map {
      case term if term.typeSignature <:< ru.typeOf[Option[Any]] =>
        val termName = term.name.toString
        if (bindings.contains(termName)) {
          bindings.get(termName).map {
            case vl: Option[Any] => ReflectionHelper.castValue(vl, term.typeSignature)
            case vl => ReflectionHelper.castValue(vl, term.typeSignature.typeArgs.head)
          }
        } else defaultConstructorDefaultParamValues(termName)()
      case term =>
        val termName = term.name.toString
        if (bindings.contains(termName)) {
          ReflectionHelper.castValue(bindings(termName), term.typeSignature)
        } else defaultConstructorDefaultParamValues(termName)()
    }

    buildWith(builtArgs)
  }
}
