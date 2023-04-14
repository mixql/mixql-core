package org.mixql.core.context

import org.mixql.core.function.SqlLambda
import scala.collection.mutable.{Map => MutMap}

package object gtype {
  import scala.language.implicitConversions

  /** implicit conversions from scala types to gtypes or gtypes to scala types
    */
  object implicitGtypeConversions {
    implicit def from_int(a: Int): gInt = gInt(a)

    implicit def to_int(a: gInt): Int = a.value

    implicit def from_double(a: Double): gDouble = gDouble(a)

    implicit def to_double(a: gDouble): Double = a.value

    implicit def from_bool(a: Boolean): bool = bool(a)

    implicit def to_bool(a: bool): Boolean = a.value

    implicit def from_string(a: String): string = string(a)

    implicit def to_string(a: string): String = a.value

    // implicit def from_array(a: Array[Any]): array = array(a)

    // implicit def to_array(a: array): Array[Any] = a.arr
  }

  /** conversions any gtype to gtype you need (if possible)
    */
  object typeConversion {
    import implicitGtypeConversions._

    def to_string(a: Type): string = {
      string(a.toString)
    }

    def to_bool(a: Type): bool = {
      a match {
        case value: bool   => value
        case gDouble(value) => value != 0
        case gInt(value)    => value != 0
        case string(value, _) =>
          if (value.toLowerCase == "true")
            true
          else if (value.toLowerCase == "false")
            false
          else {
            val quot = """""""
            throw new ClassCastException(
              s"cannot convert string " + quot + value + quot + " to bool" // bug in scala 2_12
            )
          }
        case Null =>
          throw new NullPointerException("cannot convert null to bool")
        case value =>
          throw new ClassCastException(
            s"cannot convert ${value.getClass.getSimpleName} to bool"
          )
      }
    }

    def to_int(a: Type): gInt = {
      a match {
        case bool(value)      => if (value) 1 else 0
        case gDouble(value)    => gInt(value.toInt)
        case value: gInt       => value
        case string(value, _) => gInt(value.toInt)
        case Null =>
          throw new NullPointerException("cannot convert null to int")
        case value =>
          throw new ClassCastException(
            s"cannot convert ${value.getClass.getSimpleName} to int"
          )
      }
    }

    def to_double(a: Type): gDouble = {
      a match {
        case bool(value)      => if (value) 1 else 0
        case value: gDouble    => value
        case gInt(value)       => value.toDouble
        case string(value, _) => value.toDouble
        case Null =>
          throw new NullPointerException("cannot convert null to double")
        case value =>
          throw new ClassCastException(
            s"cannot convert ${value.getClass.getSimpleName} to double"
          )
      }
    }
  }

  def isNull(a: Type): Boolean =
    a match {
      case Null => true
      case _    => false
    }

  def pack(a: Any): Type = {
    a match {
      case null          => Null
      case p: String     => string(p)
      case p: Int        => gInt(p)
      case p: Double     => gDouble(p)
      case p: Boolean    => bool(p)
      case p: Array[Any] => array(p.map(pack))
      case p: Map[Any, Any] =>
        map(
          collection.mutable
            .Map(p.map(kv => pack(kv._1) -> pack(kv._2)).toSeq: _*)
        )
      case p: SqlLambda => p
      case other        => string(other.toString)
    }
  }

  def unpack(a: Type): Any = {
    a match {
      case Null         => null
      case string(v, q) => v
      case gInt(v)       => v
      case gDouble(v)    => v
      case bool(v)      => v
      case array(v)     => v.map(unpack)
      case map(v)       => v.map(kv => unpack(kv._1) -> unpack(kv._2)).toMap
      case v: SqlLambda => v
    }
  }
}
