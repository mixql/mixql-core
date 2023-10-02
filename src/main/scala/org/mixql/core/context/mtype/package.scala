package org.mixql.core.context

import org.mixql.core.function.MLambda
import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.exception.MException
import scala.concurrent.Future

package object mtype {
  import scala.language.implicitConversions

  /** implicit conversions from scala types to mtypes or mtypes to scala types
    */
  object implicitMtypeConversions {
    implicit def from_int(a: Int): MInt = new MInt(a)
    implicit def from_int(a: Long): MInt = new MInt(a)

    implicit def to_int(a: MInt): Long = a.getValue

    implicit def from_double(a: Double): MDouble = new MDouble(a)

    implicit def to_double(a: MDouble): Double = a.getValue

    implicit def from_bool(a: Boolean): MBool = new MBool(a)

    implicit def to_bool(a: MBool): Boolean = a.getValue

    implicit def from_string(a: String): MString = new MString(a)

    implicit def to_string(a: MString): String = a.getValue

    // implicit def from_array(a: Array[Any]): array = array(a)

    // implicit def to_array(a: array): Array[Any] = a.arr
  }

  /** conversions any mtype to mtype you need (if possible)
    */
  object typeConversion {
    import implicitMtypeConversions._

    def to_string(a: MType): MString = { new MString(a.toString) }

    def to_bool(a: MType): MBool = {
      a match {
        case value: MBool => value
        case t: MDouble  => t.getValue != 0
        case t: MInt     => t.getValue != 0
        case t: MString =>
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
        case _: MNull => throw new NullPointerException("cannot convert null to bool")
        case value   => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to bool")
      }
    }

    def to_int(a: MType): MInt = {
      a match {
        case t: MBool =>
          if (t.getValue)
            1
          else
            0
        case t: MDouble  => new MInt(t.getValue.toInt)
        case value: MInt => value
        case t: MString   => new MInt(t.getValue.toInt)
        case _: MNull     => throw new NullPointerException("cannot convert null to int")
        case value       => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to int")
      }
    }

    def to_double(a: MType): MDouble = {
      a match {
        case t: MBool =>
          if (t.getValue)
            1
          else
            0
        case value: MDouble => value
        case t: MInt        => t.getValue.toDouble
        case t: MString      => t.getValue.toDouble
        case _: MNull        => throw new NullPointerException("cannot convert null to double")
        case value          => throw new ClassCastException(s"cannot convert ${value.getClass.getSimpleName} to double")
      }
    }
  }

  def isNull(a: MType): Boolean =
    a match {
      case _: MNull => true
      case _       => false
    }

  def isNone(a: MType): Boolean =
    a match {
      case _: MNone => true
      case _       => false
    }

  def pack(a: Any): MType = {
    a match {
      case p: MType       => p
      case null          => new MNull()
      case p: String     => new MString(p)
      case p: Int        => new MInt(p)
      case p: Long       => new MInt(p)
      case p: Double     => new MDouble(p)
      case p: Boolean    => new MBool(p)
      case p: Array[Any] => new MArray(p.map(pack))
      case p: Map[Any, Any] =>
        import scala.collection.JavaConverters._
        new MMap(scala.collection.mutable.Map(p.map(kv => pack(kv._1) -> pack(kv._2)).toSeq: _*).asJava)
      case p: Future[Any] => new MAsync(p)
      case other          => new MString(other.toString)
    }
  }

  def unpack(a: MType): Any = {
    import scala.collection.JavaConverters._
    a match {
      case e: MException => e
      case _: MNull             => null
      case t: MString           => t.getValue
      case t: MInt             => t.getValue
      case t: MDouble          => t.getValue
      case t: MBool             => t.getValue
      case t: MArray            => t.getArr.map(unpack)
      case t: MMap              => t.getMap.asScala.map(kv => unpack(kv._1) -> unpack(kv._2)).toMap
      case v: MLambda        => v
      case a: MAsync         => a.fut
    }
  }
}
