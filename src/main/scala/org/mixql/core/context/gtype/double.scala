package org.mixql.core.context.gtype

case class double(value: Double) extends Type {
  def this(v: String) = this(v.toDouble)

  override def toString: String = value.toString

  override def +(other: Type): Type =
    other match {
      case int(oval)    => double(value + oval)
      case double(oval) => double(value + oval)
      case array(arr)   => array(this +: arr)
      case oval: string => string(value.toString + other.toString, oval.quote)
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
