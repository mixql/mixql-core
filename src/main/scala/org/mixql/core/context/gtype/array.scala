package org.mixql.core.context.gtype

case class array(arr: Array[Type]) extends collection {
  override def toString: String = {
    arr
      .map(v => {
        v match {
          case str: string => str.asLiteral
          case other       => other.toString
        }
      })
      .mkString("[", ", ", "]")
  }

  override def +(other: Type): Type =
    other match {
      case array(v) => array(arr ++ v)
      case obj      => array(arr :+ obj)
    }

  override def ==(other: Type): Type =
    other match {
      case array(other) => bool(arr == other)
      case _            => bool(false)
    }

  override def size: int = int(arr.size)

  override def apply(index: Type): Type =
    index match {
      case int(i) => arr(i)
      case _ => throw new IllegalArgumentException("array index must be int")
    }

  override def update(index: Type, value: Type): Unit =
    index match {
      case int(i) => arr.update(i, value)
      case _ => throw new IllegalArgumentException("array index must be int")
    }
}
