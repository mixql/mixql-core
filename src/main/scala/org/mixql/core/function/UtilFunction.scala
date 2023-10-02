package org.mixql.core.function

import org.mixql.core.context.Context
import org.mixql.core.exception.MException
import scala.concurrent.Future
import org.mixql.core.context.mtype.MAsync

object UtilFunction {

  val is_error =
    new (Any => Boolean) {
      override def apply(value: Any): Boolean = value.isInstanceOf[MException]
    }

  val await =
    new (Future[Any] => Any) {

      override def apply(value: Future[Any]): Any = {
        new MAsync(value).await()
      }
    }
}
