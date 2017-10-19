package com.github.slowaner.scala.reflections4s

import java.time.LocalDateTime

import scala.reflect.api.{Mirror, TypeCreator, Universe}
import scala.reflect.runtime.{currentMirror => cm, universe => ru}

object ReflectionHelper {
  // Shortcuts for java classes
  type JavaInteger = java.lang.Integer
  type JavaBoolean = java.lang.Boolean

  //// TypeTags
  // Scala classes TypeTags
  val IntTypeTag: ru.TypeTag[Int] = implicitly[ru.TypeTag[Int]]
  val StringTypeTag: ru.TypeTag[String] = implicitly[ru.TypeTag[String]]
  val BooleanTypeTag: ru.TypeTag[Boolean] = implicitly[ru.TypeTag[Boolean]]
  val OptionTypeTag: ru.TypeTag[Option[_]] = implicitly[ru.TypeTag[Option[_]]]

  // Java classes TypeTags
  val IntegerTypeTag: ru.TypeTag[JavaInteger] = implicitly[ru.TypeTag[JavaInteger]]
  val JavaBooleanTypeTag: ru.TypeTag[JavaBoolean] = implicitly[ru.TypeTag[JavaBoolean]]
  val LocalDateTimeTypeTag: ru.TypeTag[LocalDateTime] = implicitly[ru.TypeTag[LocalDateTime]]


  //// Types
  // Scala classes Types
  val IntType: ru.Type = IntTypeTag.tpe
  val StringType: ru.Type = StringTypeTag.tpe
  val BooleanType: ru.Type = BooleanTypeTag.tpe
  val OptionType: ru.Type = OptionTypeTag.tpe

  // Java classes Types
  val IntegerType: ru.Type = IntegerTypeTag.tpe
  val JavaBooleanType: ru.Type = JavaBooleanTypeTag.tpe
  val LocalDateTimeType: ru.Type = LocalDateTimeTypeTag.tpe

  def typeToTypeTag(tpe: ru.Type, mirror: Mirror[ru.type] = cm): ru.TypeTag[_] = ru.TypeTag(cm, new TypeCreator {
    override def apply[U <: Universe with Singleton](m: Mirror[U]): U#Type = {
      assert(m == mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
      tpe.asInstanceOf[U#Type]
    }
  })

  final def castValue(value: Any, castTo: ru.Type): Any = {
    castTo match {
      case t if t =:= IntType => toInt(value)
      case t if t =:= StringType => toStr(value)
      case t if t =:= BooleanType => toBoolean(value)
      case t if t =:= JavaBooleanType => toJavaBoolean(value)
      case t if t =:= IntegerType => toInteger(value)
      case t if t =:= LocalDateTimeType => toLocalDateTime(value)
      case t if t =:= OptionType => toOption(value, castTo)
      case _ => value
    }
  }

  private final def toBoolean: PartialFunction[Any, Boolean] = {
    case x: Boolean => x
    case x: JavaBoolean => x
    case x: String => x.toBoolean
  }

  private final def toOption(value: Any, optionTpe: ru.Type): Option[_] = {
    val argTpe = optionTpe match {
      case x if x <:< ru.typeOf[Option[Any]] => x.typeArgs.head
      case x => x
    }
    value match {
      case None => None
      case Some(x) => Some(castValue(x, argTpe))
      case x => Some(castValue(x, argTpe))
    }
  }

  private final def toJavaBoolean: PartialFunction[Any, JavaBoolean] = {
    case x: JavaBoolean => x
    case x: Boolean => x
    case x: String => x.toBoolean
    case null => null
  }

  private final def toInteger: PartialFunction[Any, JavaInteger] = {
    case x: JavaInteger => x
    case x: Int => x
    case x: BigInt => x.toInt
    case null => null
  }

  private final def toInt: PartialFunction[Any, Int] = {
    case x: Int => x
    case x: JavaInteger => x.toInt
    case x: BigInt => x.toInt
  }

  private final def toStr: PartialFunction[Any, String] = {
    case x: String => x
    case null => null
    case x => x.toString
  }

  private final def toLocalDateTime: PartialFunction[Any, LocalDateTime] = {
    case x: LocalDateTime => x
    case x: String => LocalDateTime.parse(x)
    case null => null
  }
}