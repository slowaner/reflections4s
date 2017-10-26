package com.github.slowaner.scala.reflections4s

import scala.reflect.runtime.{universe => ru}

trait ClassFactory[T] {
  /**
    * Type tag for further reflect class construction
    */
  val ttag: ru.TypeTag[T]

  /**
    * Attempts to create a new instance of the specified type by calling the
    * constructor method with the supplied arguments.
    *
    * @param args the arguments to supply to the constructor method
    *
    * @return constructed object
    */
  protected def buildWith(args: Seq[_]): T

  /**
    * <p>Constructs new object by input parameters</p>
    * <p>Parameters to constructor are selected from `bindings` parameter</p>
    *
    * @param bindings Map of input parameters.
    *
    * @return constructed object
    */
  def buildWith(bindings: Map[String, Any]): T
}

object ClassFactory {
  def apply[T](implicit ttag: ru.TypeTag[T]): ClassFactory[T] =
    if (ttag.tpe.typeSymbol.asClass.isCaseClass) CaseClassFactory[T](ttag)
    else CommonClassFactory[T](ttag)
}