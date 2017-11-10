package com.github.slowaner.scala.reflections4s

import scala.language.postfixOps
import scala.reflect.runtime.{universe => ru}

case class CommonClassFactory[T](ttag: ru.TypeTag[T]) extends ClassFactory[T] {

  val classMirror: ru.ClassMirror = mirror reflectClass classSymbol

  val constructorSymbol: ru.Symbol = tpe decl ru.termNames.CONSTRUCTOR

  val defaultConstructorSymbol: ru.MethodSymbol =
    if (constructorSymbol isMethod) constructorSymbol asMethod
    else constructorSymbol.asTerm.alternatives map (_.asMethod) find (_.isPrimaryConstructor) get

  val defaultConstructorParams: List[ru.Symbol] = defaultConstructorSymbol.paramLists flatten

  val defaultConstructorMethod: ru.MethodMirror = classMirror reflectConstructor defaultConstructorSymbol

  val defaultConstructorDefaultParamValues: Map[String, ru.MethodMirror] = if (hasCompanion)
    defaultConstructorParams.zipWithIndex map {
      case (symbol, index) =>
        val name = symbol.name toString
        val defarg = moduleInstanceMirrorType member ru.TermName(s"$$lessinit$$greater$$default$$${index + 1}")
        val reflectedMethod = if (defarg != ru.NoSymbol) {
          moduleInstanceMirror reflectMethod defarg.asMethod
        } else null
        name -> reflectedMethod
    } filterNot (_._2 == null) toMap
  else Map.empty

  override protected def buildWith(args: Seq[_]): T = defaultConstructorMethod(args: _*).asInstanceOf[T]

  override def buildWith(bindings: Map[String, Any]): T = {
    if (defaultConstructorParams exists (tp => !(bindings.contains(tp.name.toString)
      || defaultConstructorDefaultParamValues.contains(tp.name.toString)
      || tp.typeSignature <:< ru.typeOf[Option[Any]]))) throw new Exception("Not enough actual parameters")
    val builtArgs = defaultConstructorParams map {
      case term if term.typeSignature <:< ru.typeOf[Option[Any]] =>
        val termName = term.name toString
        val result = if (bindings contains termName) bindings get termName map (ReflectionHelper castValue(_, term.typeSignature))
        else defaultConstructorDefaultParamValues(termName)()
        result
      case term =>
        val termName = term.name toString
        val result = if (bindings.contains(termName)) ReflectionHelper castValue(bindings(termName), term.typeSignature)
        else defaultConstructorDefaultParamValues(termName)()
        result
    }

    buildWith(builtArgs)
  }
}
