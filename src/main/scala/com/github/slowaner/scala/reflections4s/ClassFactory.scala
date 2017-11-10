package com.github.slowaner.scala.reflections4s

import scala.language.postfixOps
import scala.reflect.runtime.{universe => ru}

trait ClassFactory[T] {
  /**
    * Type tag for further reflect class construction
    */
  def ttag: ru.TypeTag[T]

  /**
    * Mirror for TypeTag `ttag`
    */
  val mirror: ru.Mirror = ttag.mirror
  /**
    * Type of TypeTag `ttag`
    */
  val tpe: ru.Type = ttag.tpe
  /**
    * ClassSymbol of Type `tpe`
    */
  val classSymbol: ru.ClassSymbol = tpe.typeSymbol asClass
  /**
    * Defines if this ClassSymbol `classSymbol` has companion
    */
  val hasCompanion: Boolean = classSymbol.companion != ru.NoSymbol
  /**
    * ModuleSymbol for ClassSymbol `classSymbol`
    */
  val moduleSymbol: ru.ModuleSymbol = if (hasCompanion) classSymbol.companion asModule else null
  /**
    * ModuleInstance for ModuleSymbol `moduleSymbol`
    */
  val moduleInstance: Any = if (hasCompanion) (mirror reflectModule moduleSymbol) instance else null
  /**
    * InstanceMirror for ModuleInstance `moduleInstance`
    */
  val moduleInstanceMirror: ru.InstanceMirror = if (hasCompanion) mirror reflect moduleInstance else null
  /**
    * TypeSignatyre (Type) of Module InstanceMirror `moduleInstanceMirror`
    */
  val moduleInstanceMirrorType: ru.Type = if (hasCompanion) moduleInstanceMirror.symbol typeSignature else null
  /**
    * Defines if this ClassFactory built for case class
    */
  val isCaseClass: Boolean = classSymbol isCaseClass

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