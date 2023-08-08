package org.mixql.core.context

import org.mixql.core.function.SqlLambda
import scala.collection.mutable.{Map => MutMap}

package object gtype {
  import scala.language.implicitConversions

  /** implicit conversions from scala types to gtypes or gtypes to scala types
    */
  object implicitGtypeConversions {
    implicit def from_int(a: Int): gInt = new gInt(a)

    implicit def to_int(a: gInt): Int = a.getValue

    implicit def from_double(a: Double): gDouble = new gDouble(a)

    implicit def to_double(a: gDouble): Double = a.getValue

    implicit def from_bool(a: Boolean): bool = new bool(a)

    implicit def to_bool(a: bool): Boolean = a.getValue

    implicit def from_string(a: String): string = new string(a)

    implicit def to_string(a: string): String = a.getValue

    // implicit def from_array(a: Array[Any]): array = array(a)

    // implicit def to_array(a: array): Array[Any] = a.arr
  }

  /** conversions any gtype to gtype you need (if possible)
    */
  object typeConversion {
    import implicitGtypeConversions._

    def to_string(a: Type): string = { new string(a.toString) }

    def to_bool(a: Type): bool = {
      a match {
        case value: bool => value
        case t: gDouble  => t.getValue != 0
        case t: gInt     => t.getValue != 0
        case t: string =>
          if (t.getValue.toLowerCase == "true")
            true
          else if (t.getValue.toLowerCase == "false")
            false
          else {
            val quot = """""""
            throw new ClassCastException(
              s"cannot convert string " + quot + t.getValue + quot +
                " to bool" // bug in scala 2_12
            )
          }
        case _: Null => throw new NullPointerException("cannot convert null to bool")
        case value   => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to bool")
      }
    }

    def to_int(a: Type): gInt = {
      a match {
        case t: bool =>
          if (t.getValue)
            1
          else
            0
        case t: gDouble  => new gInt(t.getValue.toInt)
        case value: gInt => value
        case t: string   => new gInt(t.getValue.toInt)
        case _: Null     => throw new NullPointerException("cannot convert null to int")
        case value       => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to int")
      }
    }

    def to_double(a: Type): gDouble = {
      a match {
        case t: bool =>
          if (t.getValue)
            1
          else
            0
        case value: gDouble => value
        case t: gInt        => t.getValue.toDouble
        case t: string      => t.getValue.toDouble
        case _: Null        => throw new NullPointerException("cannot convert null to double")
        case value          => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to double")
      }
    }
  }

  def isNull(a: Type): Boolean =
    a match {
      case _: Null => true
      case _       => false
    }

  def isNone(a: Type): Boolean =
    a match {
      case _: none => true
      case _       => false
    }

  def pack(a: Any): Type = {
    a match {
      case null          => new Null()
      case p: String     => new string(p)
      case p: Int        => new gInt(p)
      case p: Double     => new gDouble(p)
      case p: Boolean    => new bool(p)
      case p: Array[Any] => new array(p.map(pack))
      case p: Map[Any, Any] =>
        import scala.collection.JavaConverters._
        new map(scala.collection.mutable.Map(p.map(kv => pack(kv._1) -> pack(kv._2)).toSeq: _*).asJava)
      case p: SqlLambda => p
      case other        => new string(other.toString)
    }
  }

  def unpack(a: Type): Any = {
    import scala.collection.JavaConverters._
    a match {
      case _: Null      => null
      case t: string    => t.getValue
      case t: gInt      => t.getValue
      case t: gDouble   => t.getValue
      case t: bool      => t.getValue
      case t: array     => t.getArr.map(unpack)
      case t: map       => t.getMap.asScala.map(kv => unpack(kv._1) -> unpack(kv._2)).toMap
      case v: SqlLambda => v
    }
  }
}
