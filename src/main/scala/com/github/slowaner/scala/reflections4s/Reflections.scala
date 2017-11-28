package com.github.slowaner.scala
package reflections4s

import java.lang.annotation.Annotation
import java.util

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.{universe => ru}

import org.reflections.scanners.{Scanner => JScanner}
import org.reflections.util.{ConfigurationBuilder => JConfigurationBuilder}
import org.reflections.{Configuration => JConfiguration, Reflections => JReflections}

/**
  * Created by slowaner on 19.07.2017.
  */
class Reflections(configuration: JConfiguration) extends JReflections(configuration) {
  val mirror: ru.Mirror = scala.reflect.runtime.currentMirror

  def this(params: AnyRef*) = this(JConfigurationBuilder.build(params: _*))

  def this(prefix: String, scanners: JScanner*) = this(Seq[AnyRef](prefix) ++ scanners: _*)

  def this() = this(new JConfigurationBuilder())


  override protected def getSubTypesOf[T](cls: Class[T]): util.Set[Class[_ <: T]] = super.getSubTypesOf(cls)

  def getSubClassesOf[T](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[Class[_ <: T]] = this.getSubTypesOf[T](ctag.runtimeClass.asInstanceOf[Class[T]]).asScala.toSet

  def getSubTypesOf[T](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[ru.Type] =
    this.getSubClassesOf[T].map((cls1: Class[_]) => mirror.classSymbol(cls1).selfType).filter(_ <:< ttag.tpe)

  def getWeakSubTypesOf[T](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[ru.Type] =
    this.getSubClassesOf[T].map((cls1: Class[_]) => mirror.classSymbol(cls1).selfType).filter(_ weak_<:< ttag.tpe)

  def getCompanionsAreSubTypeOf[T](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[T] =
    this.getSubTypesOf[T].map(tp => {
      if (tp.termSymbol.isModule) mirror.reflectModule(tp.termSymbol.asModule).instance.asInstanceOf[T]
      else null.asInstanceOf[T]
    }).filter(_ != null)

  def getCompanionsAreWeakSubTypeOf[T](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[T] =
    this.getWeakSubTypesOf[T].map(tp => {
      if (tp.termSymbol.isModule) mirror.reflectModule(tp.termSymbol.asModule).instance.asInstanceOf[T]
      else null.asInstanceOf[T]
    }).filter(_ != null)

  override protected def getTypesAnnotatedWith(annotation: Class[_ <: Annotation]): util.Set[Class[_]] = super.getTypesAnnotatedWith(annotation)

  def getClassesAnnotatedWith[T <: Annotation](implicit ctag: ClassTag[T], ttag: TypeTag[T]): Set[Class[_]] = this.getTypesAnnotatedWith(ctag.runtimeClass.asInstanceOf[Class[T]]).asScala.toSet
}
