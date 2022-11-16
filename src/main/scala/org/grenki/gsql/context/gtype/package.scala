package org.grenki.gsql.context

package object gtype {
  import scala.language.implicitConversions

  /** implicit conversions from scala types to gtypes or gtypes to scala types
    */
  object implicitGtypeConversions {
    implicit def from_int(a: Int): int = int(a)

    implicit def to_int(a: int): Int = a.value

    implicit def from_double(a: Double): double = double(a)

    implicit def to_double(a: double): Double = a.value

    implicit def from_bool(a: Boolean): bool = bool(a)

    implicit def to_bool(a: bool): Boolean = a.value

    implicit def from_string(a: String): string = string(a)

    implicit def to_string(a: string): String = a.value
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
        case double(value) => value != 0
        case int(value)    => value != 0
        case string(value, _) =>
          if (value.toLowerCase == "true")
            true
          else if (value.toLowerCase == "false")
            false
          else
            throw new ClassCastException(
              s"cannot convert string \"$value\" to bool"
            )
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
        case bool(value)      => if (value) 1 else 0
        case double(value)    => int(value.toInt)
        case value: int       => value
        case string(value, _) => int(value.toInt)
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
        case bool(value)      => if (value) 1 else 0
        case value: double    => value
        case int(value)       => value.toDouble
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
}
