package org.mixql.core.context.gtype

case class string(value: String, quote: String = "") extends Type {
  override def toString: String = value

  def quoted: String = quote + value + quote

  def asLiteral: String = {
    val q = if (quote != "") quote else "\""
    q + value + q
  }

  override def hashCode = value.hashCode

  override def equals(other: Any) = other match {
    case o: string => value == o.value
    case _         => false // TODO mb String too?
  }

  override def +(other: Type): Type = {
    other match {
      case string(oval, oq) =>
        val q = if (quote != "") quote else oq
        string(value + oval, q)
      case array(arr) =>
        array(this +: arr)
      case oval =>
        string(value + other.toString, quote)
    }
  }

  // TODO attention do we need type check?
  override def ==(other: Type): Type =
    bool(value == other.toString)

  // TODO attention do we need type check?
  override def !=(other: Type): Type =
    bool(value != other.toString)
}