package org.grenki.gsql.context.gtype

import scala.language.implicitConversions

package object gtype {
  implicit def from_int(a: Int): int = {
    int(a)
  }

  implicit def to_int(a: int): Int = {
    a.value
  }

  implicit def from_double(a: Double): double = {
    double(a)
  }

  implicit def to_double(a: double): Double = {
    a.value
  }

  implicit def from_bool(a: Boolean): bool = {
    bool(a)
  }

  implicit def to_bool(a: bool): Boolean = {
    a.value
  }

  implicit def from_string(a: String): string = {
    string(a)
  }

  implicit def to_string(a: string): String = {
    a.value
  }
}

abstract class Type {
  def +(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} + ${other.getClass.getSimpleName} is unsupported"
  )
  def -(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} - ${other.getClass.getSimpleName} is unsupported"
  )
  def *(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} * ${other.getClass.getSimpleName} is unsupported"
  )
  def /(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} / ${other.getClass.getSimpleName} is unsupported"
  )
  def ||(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} and ${other.getClass.getSimpleName} is unsupported"
  )
  def &&(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} or ${other.getClass.getSimpleName} is unsupported"
  )
  def <(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} < ${other.getClass.getSimpleName} is unsupported"
  )
  def <=(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} <= ${other.getClass.getSimpleName} is unsupported"
  )
  def >(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} > ${other.getClass.getSimpleName} is unsupported"
  )
  def >=(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} >= ${other.getClass.getSimpleName} is unsupported"
  )
  def ==(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} == ${other.getClass.getSimpleName} is unsupported"
  )
  def !=(other: Type): Type = throw new UnsupportedOperationException(
    s"type error: ${this.getClass.getSimpleName} != ${other.getClass.getSimpleName} is unsupported"
  )
  def !(): Type = throw new UnsupportedOperationException(
    s"type error: operation `not` for ${this.getClass.getSimpleName} is unsupported"
  )
}

case object Null extends Type {
  override def toString: String = "null"

  override def +(other: Type): Type = other
}

case class bool(value: Boolean) extends Type {
  override def toString: String = value.toString

  override def !() = bool(!value)

  override def ==(other: Type): Type =
    other match {
      case bool(oval) => bool(value == oval)
      case _          => super.==(other)
    }
  override def !=(other: Type): Type =
    other match {
      case bool(oval) => bool(value != oval)
      case _          => super.!=(other)
    }
  override def ||(other: Type): Type =
    other match {
      case bool(oval) => bool(value || oval)
      case _          => super.||(other)
    }
  override def &&(other: Type): Type =
    other match {
      case bool(oval) => bool(value && oval)
      case _          => super.&&(other)
    }
}

case class int(value: Int) extends Type {
  def this(v: String) = this(v.toInt)

  override def toString: String = value.toString

  override def +(other: Type): Type =
    other match {
      case int(oval)    => int(value + oval)
      case double(oval) => double(value + oval)
      case _            => super.+(other)
    }

  override def -(other: Type): Type =
    other match {
      case int(oval)    => int(value - oval)
      case double(oval) => double(value - oval)
      case _            => super.-(other)
    }

  override def *(other: Type): Type =
    other match {
      case int(oval)    => int(value * oval)
      case double(oval) => double(value * oval)
      case _            => super.*(other)
    }

  override def /(other: Type): Type =
    other match {
      case int(oval)    => int(value / oval)
      case double(oval) => double(value / oval)
      case _            => super./(other)
    }

  override def >(other: Type): Type =
    other match {
      case int(oval)    => bool(value > oval)
      case double(oval) => bool(value > oval)
      case _            => super.>(other)
    }
  override def >=(other: Type): Type =
    other match {
      case int(oval)    => bool(value >= oval)
      case double(oval) => bool(value >= oval)
      case _            => super.>=(other)
    }
  override def <(other: Type): Type =
    other match {
      case int(oval)    => bool(value < oval)
      case double(oval) => bool(value < oval)
      case _            => super.<(other)
    }
  override def <=(other: Type): Type =
    other match {
      case int(oval)    => bool(value <= oval)
      case double(oval) => bool(value <= oval)
      case _            => super.<=(other)
    }
  override def ==(other: Type): Type =
    other match {
      case int(oval)    => bool(value == oval)
      case double(oval) => bool(value == oval)
      case _            => super.==(other)
    }
  override def !=(other: Type): Type =
    other match {
      case int(oval)    => bool(value != oval)
      case double(oval) => bool(value != oval)
      case _            => super.!=(other)
    }
}

case class double(value: Double) extends Type {
  def this(v: String) = this(v.toDouble)

  override def toString: String = value.toString

  override def +(other: Type): Type =
    other match {
      case int(oval)    => double(value + oval)
      case double(oval) => double(value + oval)
      case _            => super.+(other)
    }

  override def -(other: Type): Type =
    other match {
      case int(oval)    => double(value - oval)
      case double(oval) => double(value - oval)
      case _            => super.-(other)
    }

  override def *(other: Type): Type =
    other match {
      case int(oval)    => double(value * oval)
      case double(oval) => double(value * oval)
      case _            => super.*(other)
    }

  override def /(other: Type): Type =
    other match {
      case int(oval)    => double(value / oval)
      case double(oval) => double(value / oval)
      case _            => super./(other)
    }

  override def >(other: Type): Type =
    other match {
      case int(oval)    => bool(value > oval)
      case double(oval) => bool(value > oval)
      case _            => super.>(other)
    }
  override def >=(other: Type): Type =
    other match {
      case int(oval)    => bool(value >= oval)
      case double(oval) => bool(value >= oval)
      case _            => super.>=(other)
    }
  override def <(other: Type): Type =
    other match {
      case int(oval)    => bool(value < oval)
      case double(oval) => bool(value < oval)
      case _            => super.<(other)
    }
  override def <=(other: Type): Type =
    other match {
      case int(oval)    => bool(value <= oval)
      case double(oval) => bool(value <= oval)
      case _            => super.<=(other)
    }
  override def ==(other: Type): Type =
    other match {
      case int(oval)    => bool(value == oval)
      case double(oval) => bool(value == oval)
      case _            => super.==(other)
    }
  override def !=(other: Type): Type =
    other match {
      case int(oval)    => bool(value != oval)
      case double(oval) => bool(value != oval)
      case _            => super.!=(other)
    }
}

case class string(value: String, quote: String = "") extends Type {
  override def toString: String = value

  def quoted = quote + value + quote
  override def +(other: Type): Type =
    string(value + other.toString)

  // TODO attention do we need type check?
  override def ==(other: Type): Type =
    bool(value == other.toString)

  // TODO attention do we need type check?
  override def !=(other: Type): Type =
    bool(value != other.toString)
}
