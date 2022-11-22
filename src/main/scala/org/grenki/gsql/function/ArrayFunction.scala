package org.grenki.gsql.function

import org.grenki.gsql.context.gtype._

object ArrayFunction {
  val size = new (Array[Type] => Int) {
    override def apply(arr: Array[Type]): Int =
      arr.size
  }
}
