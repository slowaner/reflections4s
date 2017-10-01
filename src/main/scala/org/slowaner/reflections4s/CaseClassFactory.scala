package org.slowaner.reflections4s

import scala.reflect.runtime.{universe => ru}

final case class CaseClassFactory[R](ttag: ru.TypeTag[R]) {

  val mirror: ru.Mirror = ttag.mirror
  val tpe: ru.Type = ttag.tpe
  val classSymbol: ru.ClassSymbol = tpe.typeSymbol.asClass

  if (!classSymbol.isCaseClass)
    throw new IllegalArgumentException(
      "CaseClassDeserializer only applies to case classes!"
    )

  val classMirror: ru.ClassMirror = mirror.reflectClass(classSymbol)

  val constructorSymbol: ru.Symbol = tpe.decl(ru.termNames.CONSTRUCTOR)

  val defaultConstructor: ru.MethodSymbol =
    if (constructorSymbol.isMethod) constructorSymbol.asMethod
    else constructorSymbol.asTerm.alternatives.map {
      _.asMethod
    }.find {
      _.isPrimaryConstructor
    }.get


  val constructorMethod: ru.MethodMirror = classMirror reflectConstructor defaultConstructor

  /**
    * Attempts to create a new instance of the specified type by calling the
    * constructor method with the supplied arguments.
    *
    * @param args the arguments to supply to the constructor method
    */
  private[this] def buildWith(args: Seq[_]): R = constructorMethod(args: _*).asInstanceOf[R]

  def deserializeWith(bindings: Map[String, Any]): R = {
    if (defaultConstructor.typeParams.exists(tp => !(bindings.contains(tp.name.toString.trim) || tp.typeSignature <:< ru.typeOf[Option[Any]]))) throw new Exception("Not enough actual parameters")
    val buildedArgs = defaultConstructor.paramLists match {
      case List(fields) => fields.map({
        case tps if tps.typeSignature <:< ru.typeOf[Option[Any]] => bindings.get(tps.name.toString.trim).map(vl => ReflectionHelper.castValue(vl, tps.typeSignature.typeArgs.head))
        case tps => ReflectionHelper.castValue(bindings(tps.name.toString.trim), tps.typeSignature)
      })
      case Nil => Nil
    }

    buildWith(buildedArgs)
  }
}