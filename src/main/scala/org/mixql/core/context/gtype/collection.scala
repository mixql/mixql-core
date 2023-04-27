package org.mixql.core.context.gtype

abstract class collection extends Type {
  def apply(index: Type): Type
  def update(index: Type, value: Type): Unit
  def size: int
}
