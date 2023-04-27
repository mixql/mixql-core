package org.mixql.core.context.gtype

case object Null extends Type {
  override def toString: String = "null"

  override def +(other: Type): Type =
    other match {
      case array(arr) =>
        array(this +: arr)
      case _ => super.+(other)
    }
}