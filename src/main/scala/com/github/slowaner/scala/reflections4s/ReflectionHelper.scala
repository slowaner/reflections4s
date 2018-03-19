package com.github.slowaner.scala
package reflections4s

import java.lang.reflect.ParameterizedType
import java.time.LocalDateTime

import scala.language.postfixOps
import scala.reflect.api.{Mirror, TypeCreator, Universe}
import scala.reflect.runtime.{currentMirror => cm, universe => ru}
import scala.reflect.{ClassTag, Manifest}

object ReflectionHelper {
  // Shortcuts for java classes
  type JavaByte = java.lang.Byte
  type JavaShort = java.lang.Short
  type JavaCharacter = java.lang.Character
  type JavaInteger = java.lang.Integer
  type JavaLong = java.lang.Long
  type JavaFloat = java.lang.Float
  type JavaDouble = java.lang.Double
  type JavaBoolean = java.lang.Boolean

  //// TypeTags
  // Scala classes TypeTags
  // Other scala typeTags
  val OptionTypeTag: ru.TypeTag[Option[_]] = implicitly[ru.TypeTag[Option[_]]]
  val StringTypeTag: ru.TypeTag[String] = implicitly[ru.TypeTag[String]]

  // Java classes TypeTags
  val JavaByteTypeTag: ru.TypeTag[JavaByte] = implicitly[ru.TypeTag[JavaByte]]
  val JavaShortTypeTag: ru.TypeTag[JavaShort] = implicitly[ru.TypeTag[JavaShort]]
  val JavaCharacterTypeTag: ru.TypeTag[JavaCharacter] = implicitly[ru.TypeTag[JavaCharacter]]
  val JavaIntegerTypeTag: ru.TypeTag[JavaInteger] = implicitly[ru.TypeTag[JavaInteger]]
  val JavaLongTypeTag: ru.TypeTag[JavaLong] = implicitly[ru.TypeTag[JavaLong]]
  val JavaFloatTypeTag: ru.TypeTag[JavaFloat] = implicitly[ru.TypeTag[JavaFloat]]
  val JavaDoubleTypeTag: ru.TypeTag[JavaDouble] = implicitly[ru.TypeTag[JavaDouble]]
  val JavaBooleanTypeTag: ru.TypeTag[JavaBoolean] = implicitly[ru.TypeTag[JavaBoolean]]

  // Other java classes TypeTags

  val LocalDateTimeTypeTag: ru.TypeTag[LocalDateTime] = implicitly[ru.TypeTag[LocalDateTime]]

  //// Types
  import ru.definitions._

  // Scala classes Types
  val StringType: ru.Type = StringTypeTag.tpe
  val OptionType: ru.Type = OptionTypeTag.tpe

  // Java classes Types
  val JavaByteType: ru.Type = JavaByteTypeTag.tpe
  val JavaShortType: ru.Type = JavaShortTypeTag.tpe
  val JavaCharacterType: ru.Type = JavaCharacterTypeTag.tpe
  val JavaIntegerType: ru.Type = JavaIntegerTypeTag.tpe
  val JavaLongType: ru.Type = JavaLongTypeTag.tpe
  val JavaFloatType: ru.Type = JavaFloatTypeTag.tpe
  val JavaDoubleType: ru.Type = JavaDoubleTypeTag.tpe
  val JavaBooleanType: ru.Type = JavaBooleanTypeTag.tpe

  // Other java classes TypeTags
  val LocalDateTimeType: ru.Type = LocalDateTimeTypeTag.tpe

  def typeToTypeTag(tpe: ru.Type, mirror: ru.Mirror = cm): ru.TypeTag[_] = ru.TypeTag(cm, new TypeCreator {
    override def apply[U <: Universe with Singleton](m: Mirror[U]): U#Type = {
      assert(m == mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
      tpe.asInstanceOf[U#Type]
    }
  })

  def classToType(classObj: Class[_], mirror: ru.Mirror = cm): ru.Type = mirror classSymbol classObj toType

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

  private def throwTransformException(value: Any, castTo: ru.Type) =
    throw new ClassCastException(s"Can't transform `$value` of ${if (value != null) value.getClass else NullClass.toString} to ${castTo.dealias.typeSymbol.fullName}")

  final def castValue(value: Any, castTo: ru.Type): Any = castTo match {
    case tpe if tpe =:= ByteTpe => toByte(value)
    case tpe if tpe =:= ShortTpe => toShort(value)
    case tpe if tpe =:= CharTpe => toChar(value)
    case tpe if tpe =:= IntTpe => toInt(value)
    case tpe if tpe =:= LongTpe => toLong(value)
    case tpe if tpe =:= FloatTpe => toFloat(value)
    case tpe if tpe =:= DoubleTpe => toDouble(value)
    case tpe if tpe =:= BooleanTpe => toBoolean(value)
    case tpe if tpe =:= UnitTpe => toUnit(value)
    case tpe if tpe =:= AnyTpe => toAny(value)
    case tpe if tpe =:= AnyValTpe => toAnyVal(value)
    case tpe if tpe =:= AnyRefTpe => toAnyRef(value)
    case tpe if tpe =:= ObjectTpe => toObject(value)
    case tpe if tpe =:= NothingTpe => toNothing(value)
    case tpe if tpe =:= NullTpe => toNull(value)
    case tpe if tpe =:= StringType => toStr(value)
    case tpe if tpe =:= JavaBooleanType => toJavaBoolean(value)
    case tpe if tpe =:= JavaIntegerType => toInteger(value)
    case tpe if tpe =:= LocalDateTimeType => toLocalDateTime(value)
    case tpe if tpe <:< OptionType => toOption(value, tpe)
    case _ => value
    //TODO Normal transformation
    //    case tpe if classToType(value.getClass) <:< tpe => value
    //    case x => throwTransformException(value, x)
  }

  private final def toByte(o: Any): Byte = o match {
    case x: Byte => x
    case x: JavaByte => x
    case x: Short => x.toByte
    case x: Char => x.toByte
    case x: Int => x.toByte
    case x: Long => x.toByte
    case x: Float => x.toByte
    case x: Double => x.toByte
    case x: Boolean => if (x) 1 else 0
    case x: JavaShort => x.toByte
    case x: JavaCharacter => x.toByte
    case x: JavaInteger => x.toByte
    case x: JavaLong => x.toByte
    case x: JavaFloat => x.toByte
    case x: JavaDouble => x.toByte
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toByte
    case x => throwTransformException(x, ByteTpe)
  }

  private final def toShort(o: Any): Short = o match {
    case x: Short => x
    case x: JavaShort => x
    case x: Byte => x.toShort
    case x: Char => x.toShort
    case x: Int => x.toShort
    case x: Long => x.toShort
    case x: Float => x.toShort
    case x: Double => x.toShort
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toShort
    case x: JavaCharacter => x.toShort
    case x: JavaInteger => x.toShort
    case x: JavaLong => x.toShort
    case x: JavaFloat => x.toShort
    case x: JavaDouble => x.toShort
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toShort
    case x => throwTransformException(x, ShortTpe)
  }

  private final def toChar(o: Any): Char = o match {
    case x: Char => x
    case x: JavaCharacter => x
    case x: Byte => x.toChar
    case x: Short => x.toChar
    case x: Int => x.toChar
    case x: Long => x.toChar
    case x: Float => x.toChar
    case x: Double => x.toChar
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toChar
    case x: JavaShort => x.toChar
    case x: JavaInteger => x.toChar
    case x: JavaLong => x.toChar
    case x: JavaFloat => x.toChar
    case x: JavaDouble => x.toChar
    case x: JavaBoolean => if (x) 1 else 0
    case x: String if x.length == 1 => x(0)
    case x => throwTransformException(x, CharTpe)
  }

  private final def toInt(o: Any): Int = o match {
    case x: Int => x
    case x: JavaInteger => x
    case x: BigInt => x.toInt
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Char => x.toInt
    case x: Long => x.toInt
    case x: Float => x.toInt
    case x: Double => x.toInt
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toInt
    case x: JavaShort => x.toInt
    case x: JavaCharacter => x.toInt
    case x: JavaLong => x.toInt
    case x: JavaFloat => x.toInt
    case x: JavaDouble => x.toInt
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toInt
    case x => throwTransformException(x, IntTpe)
  }

  private final def toLong(o: Any): Long = o match {
    case x: Long => x
    case x: JavaLong => x
    case x: Byte => x.toLong
    case x: Short => x.toLong
    case x: Char => x.toLong
    case x: Int => x.toLong
    case x: Float => x.toLong
    case x: Double => x.toLong
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toLong
    case x: JavaShort => x.toLong
    case x: JavaCharacter => x.toLong
    case x: JavaInteger => x.toLong
    case x: JavaFloat => x.toLong
    case x: JavaDouble => x.toLong
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toLong
    case x => throwTransformException(x, LongTpe)
  }

  private final def toFloat(o: Any): Float = o match {
    case x: Float => x
    case x: JavaFloat => x
    case x: Byte => x.toFloat
    case x: Short => x.toFloat
    case x: Char => x.toFloat
    case x: Int => x.toFloat
    case x: Long => x.toFloat
    case x: Double => x.toFloat
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toFloat
    case x: JavaShort => x.toFloat
    case x: JavaCharacter => x.toFloat
    case x: JavaInteger => x.toFloat
    case x: JavaLong => x.toFloat
    case x: JavaDouble => x.toFloat
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toFloat
    case x => throwTransformException(x, FloatTpe)
  }

  private final def toDouble(o: Any): Double = o match {
    case x: Double => x
    case x: JavaDouble => x
    case x: Byte => x.toDouble
    case x: Short => x.toDouble
    case x: Char => x.toDouble
    case x: Int => x.toDouble
    case x: Long => x.toDouble
    case x: Float => x.toDouble
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toDouble
    case x: JavaShort => x.toDouble
    case x: JavaCharacter => x.toDouble
    case x: JavaInteger => x.toDouble
    case x: JavaLong => x.toDouble
    case x: JavaFloat => x.toDouble
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toDouble
    case x => throwTransformException(x, DoubleTpe)
  }

  private final def toBoolean(o: Any): Boolean = o match {
    case x: Boolean => x
    case x: JavaBoolean => x
    case x: Byte => if (x == 0) false else true
    case x: Short => if (x == 0) false else true
    case x: Char => if (x == 0) false else true
    case x: Int => if (x == 0) false else true
    case x: Long => if (x == 0) false else true
    case x: Float => if (x == 0) false else true
    case x: Double => if (x == 0) false else true
    case x: JavaByte => if (x == 0) false else true
    case x: JavaShort => if (x == 0) false else true
    case x: JavaCharacter => if (x == 0) false else true
    case x: JavaInteger => if (x == 0) false else true
    case x: JavaLong => if (x == 0) false else true
    case x: JavaFloat => if (x == 0) false else true
    case x: JavaDouble => if (x == 0) false else true
    case x: String => x.toBoolean
    case x => throwTransformException(x, BooleanTpe)
  }

  private final def toUnit(o: Any): Unit = {}

  private final def toAny(o: Any): Any = o

  private final def toAnyVal(o: Any): AnyVal = o match {
    case x if ScalaPrimitiveValueClasses contains x => x.asInstanceOf[AnyVal]
    case x => throwTransformException(x, AnyValTpe)
  }

  private final def toAnyRef(o: Any): AnyRef = o match {
    case x: AnyRef => x
    case x => throwTransformException(x, AnyRefTpe)
  }

  private final def toObject(o: Any): Object = o match {
    case x: Object => x
    case x => throwTransformException(x, ObjectTpe)
  }

  /**
    * @note <b>BEWARE! `toNothing` return `Unit`! This is not correct but it's not possible to return `Nothing`.</b>
    * @param o Input value
    */
  private final def toNothing(o: Any): Unit = {}

  private final def toNull(o: Any): Null = null

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

  private final def toInteger(o: Any): JavaInteger = o match {
    case x: JavaInteger => x
    case x: Int => x
    case x: BigInt => x.toInt
    case null => null
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Char => x.toInt
    case x: Long => x.toInt
    case x: Float => x.toInt
    case x: Double => x.toInt
    case x: Boolean => if (x) 1 else 0
    case x: JavaByte => x.toInt
    case x: JavaShort => x.toInt
    case x: JavaCharacter => x.toInt
    case x: JavaLong => x.toInt
    case x: JavaFloat => x.toInt
    case x: JavaDouble => x.toInt
    case x: JavaBoolean => if (x) 1 else 0
    case x: String => x.toInt
    case x => throwTransformException(x, JavaIntegerType)
  }


  private final def toJavaBoolean(o: Any): JavaBoolean = o match {
    case x: JavaBoolean => x
    case x: Boolean => x
    case null => null
    case x: Byte => if (x == 0) false else true
    case x: Short => if (x == 0) false else true
    case x: Char => if (x == 0) false else true
    case x: Int => if (x == 0) false else true
    case x: Long => if (x == 0) false else true
    case x: Float => if (x == 0) false else true
    case x: Double => if (x == 0) false else true
    case x: JavaByte => if (x == 0) false else true
    case x: JavaShort => if (x == 0) false else true
    case x: JavaCharacter => if (x == 0) false else true
    case x: JavaInteger => if (x == 0) false else true
    case x: JavaLong => if (x == 0) false else true
    case x: JavaFloat => if (x == 0) false else true
    case x: JavaDouble => if (x == 0) false else true
    case x: String => x.toBoolean
    case x => throwTransformException(x, JavaBooleanType)
  }

  private final def toStr(o: Any): String = o match {
    case x: String => x
    case null => null
    case x => x.toString
  }

  private final def toLocalDateTime(o: Any): LocalDateTime = o match {
    case x: LocalDateTime => x
    case x: String => LocalDateTime parse x
    case null => null
    case x => throwTransformException(x, LocalDateTimeType)
  }
}