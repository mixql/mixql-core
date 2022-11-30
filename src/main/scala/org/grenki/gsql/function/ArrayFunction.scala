package org.grenki.gsql.function

import org.grenki.gsql.context.gtype._

object ArrayFunction {
  val size = new (Array[Any] => Int) {
    override def apply(arr: Array[Any]): Int =
      arr.size
  }
}
