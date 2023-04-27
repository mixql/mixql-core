package org.mixql.core.context

import org.mixql.core.function.SqlLambda
import scala.collection.mutable.{Map => MutMap}

package object gtype {
  import scala.language.implicitConversions

  /** implicit conversions from scala types to gtypes or gtypes to scala types
    */
  object implicitGtypeConversions {
    implicit def from_int(a: Int): int = new int(a)

    implicit def to_int(a: int): Int = a.value

    implicit def from_double(a: Double): double = double(a)

    implicit def to_double(a: double): Double = a.value

    implicit def from_bool(a: Boolean): bool = bool(a)

    implicit def to_bool(a: bool): Boolean = a.value

    implicit def from_string(a: String): string = new string(a)

    implicit def to_string(a: string): String = a.value

    // implicit def from_array(a: Array[Any]): array = array(a)

    // implicit def to_array(a: array): Array[Any] = a.arr
  }

  /** conversions any gtype to gtype you need (if possible)
    */
  object typeConversion {
    import implicitGtypeConversions._

    def to_string(a: Type): string = {
      new string(a.toString)
    }

    def to_bool(a: Type): bool = {
      a match {
        case value: bool   => value
        case t: double => t.value != 0
        case t: int    => t.value != 0
        case t: string =>
          if (t.value.toLowerCase == "true")
            true
          else if (t.value.toLowerCase == "false")
            false
          else {
            val quot = """""""
            throw new ClassCastException(
              s"cannot convert string " + quot + t.value + quot + " to bool" // bug in scala 2_12
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

    def to_int(a: Type): int = {
      a match {
        case t: bool      => if (t.value) 1 else 0
        case t: double    => new int(t.value.toInt)
        case value: int       => value
        case t: string => new int(t.value.toInt)
        case Null =>
          throw new NullPointerException("cannot convert null to int")
        case value =>
          throw new ClassCastException(
            s"cannot convert ${value.getClass.getSimpleName} to int"
          )
      }
    }

    def to_double(a: Type): double = {
      a match {
        case t: bool      => if (t.value) 1 else 0
        case value: double    => value
        case t: int       => t.value.toDouble
        case t: string => t.value.toDouble
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
      case p: String     => new string(p)
      case p: Int        => new int(p)
      case p: Double     => new double(p)
      case p: Boolean    => new bool(p)
      case p: Array[Any] => new array(p.map(pack))
      case p: Map[Any, Any] =>
        import scala.collection.JavaConverters._
        new map(
          scala.collection.mutable
            .Map(p.map(kv => pack(kv._1) -> pack(kv._2)).toSeq: _*)
        )
      case p: SqlLambda => p
      case other        => new string(other.toString)
    }
  }

  def unpack(a: Type): Any = {
    import scala.collection.JavaConverters._
    a match {
      case  Null         => null
      case t: string => t.value
      case t: int       => t.value
      case t: double    => t.value
      case t: bool      => t.value
      case t: array     => t.arr.map(unpack)
      case t: map       => t.m.map(kv => unpack(kv._1) -> unpack(kv._2)).toMap
      case v: SqlLambda => v
    }
  }
}
