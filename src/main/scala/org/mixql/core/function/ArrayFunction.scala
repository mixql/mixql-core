package org.mixql.core.function

import org.mixql.core.context.gtype._

object ArrayFunction {
  val size = new (Array[Any] => Int) {
    override def apply(arr: Array[Any]): Int =
      arr.size
  }

  val sort = new ((Array[Any], SqlLambda) => Array[Any]) {
    override def apply(arr: Array[Any], less: SqlLambda): Array[Any] = {
      arr.sortWith((x, y) => {
        less(x, y).asInstanceOf[Boolean]
      })
    }
  }
}
