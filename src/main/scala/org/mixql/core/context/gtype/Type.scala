package org.mixql.core.context.gtype

abstract class Type {
  var ret = false
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