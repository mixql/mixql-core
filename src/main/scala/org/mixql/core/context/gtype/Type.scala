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

case object Null extends Type {
  override def toString: String = "null"

  override def +(other: Type): Type =
    other match {
      case array(arr) =>
        array(this +: arr)
      case _ => super.+(other)
    }
}

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

case class string(value: String, quote: String = "") extends Type {
  override def toString: String = value

  def quoted: String = quote + value + quote

  def asLiteral: String = {
    val q = if quote != "" then quote else "\""
    q + value + q
  }
  override def +(other: Type): Type = {
    other match {
      case string(oval, oq) =>
        val q = if quote != "" then quote else oq
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

abstract class collection extends Type {
  def apply(index: Type): Type
  def update(index: Type, value: Type): Unit
  def size: int
}

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
