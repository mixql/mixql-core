package org.mixql.core.function

import org.mixql.core.context.gtype._
import org.mixql.core.context.Context
import org.mixql.core.exception.UserSqlException
import scala.concurrent.Future

object UtilFunction {

  val is_error =
    new (Any => Boolean) {
      override def apply(value: Any): Boolean = value.isInstanceOf[UserSqlException]
    }

  val await =
    new (Future[Any] => Any) {

      override def apply(value: Future[Any]): Any = {
        new SqlAync(value).await()
      }
    }
}
