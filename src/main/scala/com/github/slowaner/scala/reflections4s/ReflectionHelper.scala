package com.github.slowaner.scala
package reflections4s

import java.time.LocalDateTime

import scala.language.postfixOps
import scala.reflect.api.{Mirror, TypeCreator, Universe}
import scala.reflect.runtime.{currentMirror => cm, universe => ru}
import scala.reflect.{ClassTag, Manifest}

object ReflectionHelper {
  // Shortcuts for java classes
  type JavaInteger = java.lang.Integer
  type JavaBoolean = java.lang.Boolean

  //// TypeTags
  // Scala classes TypeTags
  // Other scala typeTags
  val OptionTypeTag: ru.TypeTag[Option[_]] = implicitly[ru.TypeTag[Option[_]]]
  val StringTypeTag: ru.TypeTag[String] = implicitly[ru.TypeTag[String]]

  // Java classes TypeTags
  val JavaIntegerTypeTag: ru.TypeTag[JavaInteger] = implicitly[ru.TypeTag[JavaInteger]]
  val JavaBooleanTypeTag: ru.TypeTag[JavaBoolean] = implicitly[ru.TypeTag[JavaBoolean]]
  val LocalDateTimeTypeTag: ru.TypeTag[LocalDateTime] = implicitly[ru.TypeTag[LocalDateTime]]

  //// Types
  import ru.definitions._

  // Scala classes Types
  val StringType: ru.Type = StringTypeTag.tpe
  val OptionType: ru.Type = OptionTypeTag.tpe

  // Java classes Types
  val JavaIntegerType: ru.Type = JavaIntegerTypeTag.tpe
  val JavaBooleanType: ru.Type = JavaBooleanTypeTag.tpe
  val LocalDateTimeType: ru.Type = LocalDateTimeTypeTag.tpe

  def typeToTypeTag(tpe: ru.Type, mirror: Mirror[ru.type] = cm): ru.TypeTag[_] = ru.TypeTag(cm, new TypeCreator {
    override def apply[U <: Universe with Singleton](m: Mirror[U]): U#Type = {
      assert(m == mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
      tpe.asInstanceOf[U#Type]
    }
  })

  /*def manifestFor_NOTWORKING[T](implicit ttag: ru.TypeTag[T]): Manifest[T] = {
    implicit val ct: ClassTag[T] = ClassTag[T](ttag.mirror.runtimeClass(ttag.tpe))
    manifest[T]
  }*/

  def manifestFor[T](implicit ttag: ru.TypeTag[T]): Manifest[T] = {
    val mirror = ttag.mirror

    def typeToManifest(tpe: ru.Type): Manifest[_] = tpe match {
      case ByteTpe => Manifest.Byte
      case ShortTpe => Manifest.Short
      case CharTpe => Manifest.Char
      case IntTpe => Manifest.Int
      case LongTpe => Manifest.Long
      case FloatTpe => Manifest.Float
      case DoubleTpe => Manifest.Double
      case BooleanTpe => Manifest.Boolean
      case UnitTpe => Manifest.Unit
      case AnyTpe => Manifest.Any
      case AnyValTpe => Manifest.AnyVal
      case AnyRefTpe => Manifest.AnyRef
      case ObjectTpe => Manifest.Object
      case NothingTpe => Manifest.Nothing
      case NullTpe => Manifest.Null
      case argType =>
        val rtimeClass = ClassTag(mirror.runtimeClass(argType)).runtimeClass
        //      val rtimeClass = mirror.runtimeClass(tpe).asInstanceOf[Class[T]]
        if (argType.typeArgs.nonEmpty) {
          val varargs = argType.typeArgs
          val firstTypeArgManifest = typeToManifest(varargs.head)
          val otherTypeArdManifests = varargs.tail map typeToManifest
          Manifest.classType(rtimeClass, firstTypeArgManifest, otherTypeArdManifests: _*)
        } else {
          Manifest.classType(rtimeClass)
        }
    }

    typeToManifest(ttag.tpe).asInstanceOf[Manifest[T]]
  }

  final def castValue(value: Any, castTo: ru.Type): Any = castTo match {
    case ByteTpe => toByte(value)
    case ShortTpe => toShort(value)
    case CharTpe => toChar(value)
    case IntTpe => toInt(value)
    case LongTpe => toLong(value)
    case FloatTpe => toFloat(value)
    case DoubleTpe => toDouble(value)
    case BooleanTpe => toBoolean(value)
    case UnitTpe => toUnit(value)
    case AnyTpe => toAny(value)
    case AnyValTpe => toAnyVal(value)
    case AnyRefTpe => toAnyRef(value)
    case ObjectTpe => toObject(value)
    case NothingTpe => toNothing(value)
    case NullTpe => toNull(value)
    case StringType => toStr(value)
    case JavaBooleanType => toJavaBoolean(value)
    case JavaIntegerType => toInteger(value)
    case LocalDateTimeType => toLocalDateTime(value)
    case tpe if tpe <:< OptionType => toOption(value, tpe)
    case _ => value
  }

  private final def toByte(o: Any) = o match {
    case x: Byte => x
  }

  private final def toShort(o: Any) = o match {
    case x: Short => x
  }

  private final def toChar(o: Any) = o match {
    case x: Char => x
  }

  private final def toInt(o: Any) = o match {
    case x: Int => x
    case x: JavaInteger => x toInt
    case x: BigInt => x toInt
  }

  private final def toLong(o: Any) = o match {
    case x: Long => x
    case x: Int => x.toLong
  }

  private final def toFloat(o: Any) = o match {
    case x: Float => x
  }

  private final def toDouble(o: Any) = o match {
    case x: Double => x
  }

  private final def toBoolean(o: Any) = o match {
    case x: Boolean => x
    case x: JavaBoolean => x
    case x: String => x toBoolean
  }

  private final def toUnit(o: Any): Unit = {}

  private final def toAny(o: Any) = o

  private final def toAnyVal(o: Any) = o match {
    case x if ScalaPrimitiveValueClasses contains x => x
  }

  private final def toAnyRef(o: Any) = o match {
    case x: AnyRef => x
  }

  private final def toObject(o: Any) = o match {
    case x: Object => x
  }

  /**
    * @note <b>BEWARE! `toNothing` return `Unit`! This is not correct but it's not possible to return `Nothing`.</b>
    *
    * @param o Input value
    */
  private final def toNothing(o: Any): Unit = {}

  private final def toNull(o: Any) = null

  private final def toOption(value: Any, optionTpe: ru.Type): Option[_] = {
    if (value == None) None
    else {
      val argTpe = optionTpe match {
        case x if x <:< OptionType => x.typeArgs.head
        case x => x
      }
      value match {
        case Some(x) => Some(castValue(x, argTpe))
        case x => Some(castValue(x, argTpe))
      }
    }
  }

  private final def toJavaBoolean(o: Any): JavaBoolean = o match {
    case x: JavaBoolean => x
    case x: Boolean => x
    case x: String => x toBoolean
    case null => null
  }

  private final def toInteger(o: Any): JavaInteger = o match {
    case x: JavaInteger => x
    case x: Int => x
    case x: BigInt => x toInt
    case null => null
  }

  private final def toStr(o: Any): String = o match {
    case x: String => x
    case null => null
    case x => x toString
  }

  private final def toLocalDateTime(o: Any): LocalDateTime = o match {
    case x: LocalDateTime => x
    case x: String => LocalDateTime parse x
    case null => null
  }
}