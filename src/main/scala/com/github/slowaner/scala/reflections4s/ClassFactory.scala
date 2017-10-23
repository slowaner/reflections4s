package com.github.slowaner.scala.reflections4s

import scala.reflect.runtime.{universe => ru}

final case class ClassFactory[R](ttag: ru.TypeTag[R]) {

  val mirror: ru.Mirror = ttag.mirror
  val tpe: ru.Type = ttag.tpe
  val classSymbol: ru.ClassSymbol = tpe.typeSymbol.asClass
  val moduleSymbol: ru.ModuleSymbol = classSymbol.companion.asModule
  val instanceMirror: ru.InstanceMirror = mirror reflect (mirror reflectModule moduleSymbol).instance
  val instanceMirrorType: ru.Type = instanceMirror.symbol.typeSignature
  val isCaseClass: Boolean = classSymbol.isCaseClass

  //  if (!classSymbol.isCaseClass)
  //    throw new IllegalArgumentException(
  //      "CaseClassDeserializer only applies to case classes!"
  //    )

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

  val applyMethodSymbol: ru.MethodSymbol = if (isCaseClass) (instanceMirrorType member ru.TermName("apply")).asMethod else null

  val applyMethodParams: List[ru.Symbol] = if (applyMethodSymbol != null) applyMethodSymbol.paramLists.flatten else null

  val defaultApplyMethod: ru.MethodMirror =
    if (applyMethodSymbol != null) instanceMirror reflectMethod applyMethodSymbol else null

  val defaultApplyMethodDefaultParamValues: Map[String, ru.MethodMirror] =
    if (applyMethodParams != null) applyMethodParams.zipWithIndex.map { case (symbol, index) =>
      val name = symbol.name.toString
      val defarg = instanceMirrorType member ru.TermName(s"apply$$default$$${index + 1}")
      val reflectedMethod = if (defarg != ru.NoSymbol) {
        instanceMirror reflectMethod defarg.asMethod
      } else null
      name -> reflectedMethod
    }.filterNot(_._2 == null).toMap
    else null

  /**
    * Attempts to create a new instance of the specified type by calling the
    * constructor method with the supplied arguments.
    *
    * @param args the arguments to supply to the constructor method
    */
  private[this] def buildCaseClassWith(args: Seq[_]): R = defaultApplyMethod(args: _*).asInstanceOf[R]

  private[this] def buildCommonClassWith(args: Seq[_]): R = defaultConstructorMethod(args: _*).asInstanceOf[R]

  private[this] val buildCommonClassWith: Map[String, Any] => R = bindings => {
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

    buildCommonClassWith(builtArgs)
  }

  private[this] val buildCaseClassWith: Map[String, Any] => R = bindings => {
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

    buildCaseClassWith(builtArgs)
  }

  val buildWith: Map[String, Any] => R = if (isCaseClass) buildCaseClassWith else buildCommonClassWith
}