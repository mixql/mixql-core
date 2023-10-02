package org.mixql.core.function

import org.mixql.core.context.mtype._
import org.mixql.core.context.Context

object ArrayFunction {

  val size =
    new (Array[Any] => Int) {
      override def apply(arr: Array[Any]): Int = arr.size
    }

  val sort =
    new ((Array[Any], MLambda, Context) => Array[Any]) {

      override def apply(arr: Array[Any], less: MLambda, context: Context): Array[Any] = {
        arr.sortWith((x, y) => { less(context, x, y).asInstanceOf[Boolean] })
      }
    }
}
