package org.mixql.core.context.gtype

case class bool(value: Boolean) extends Type {
  override def toString: String = value.toString

  override def +(other: Type): Type =
    other match {
      case oval: string => string(value.toString + other.toString, oval.quote)
      case array(arr)   => array(this +: arr)
      case _            => super.+(other)
    }
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
