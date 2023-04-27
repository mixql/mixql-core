package org.mixql.core.context.gtype
import scala.collection.mutable.{Map => MutMap}

case class map(m: MutMap[Type, Type]) extends collection {
  override def ==(other: Type): Type =
    other match {
      case map(other) => bool(m == other)
      case _          => bool(false)
    }

  override def apply(index: Type): Type = m(index)

  override def update(index: Type, value: Type): Unit = m.update(index, value)

  override def size: int = int(m.size)

  override def toString: String = {
    m
      .map(v => {
        val key = v._1 match {
          case str: string => str.asLiteral
          case other       => other.toString
        }
        val value = v._2 match {
          case str: string => str.asLiteral
          case other       => other.toString
        }
        key + ": " + value
      })
      .mkString("{", ", ", "}")
  }
}
