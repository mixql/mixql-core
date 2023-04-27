package org.mixql.core.context.gtype

case class int(value: Int) extends Type {
  def this(v: String) = this(v.toInt)

  override def toString: String = value.toString

  override def +(other: Type): Type =
    other match {
      case int(oval)    => int(value + oval)
      case double(oval) => double(value + oval)
      case oval: string => string(value.toString + other.toString, oval.quote)
      case array(arr)   => array(this +: arr)
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
