package org.mixql.core.context.gtype

import scala.concurrent.Future
import java.util.concurrent.ExecutionException
import org.mixql.core.exception.UserSqlException
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import java.lang.reflect.InvocationTargetException

final class MixqlAync(fut: Future[Type]) extends Type {

  def await(): Type = {
    try {
      Await.result(fut, Duration.Inf)
    } catch {
      case e: UserSqlException => e
      case e: InvocationTargetException =>
        new UserSqlException(e.getCause.getClass.getSimpleName, e.getCause.getMessage)
      case e: ExecutionException => new UserSqlException(e.getCause.getClass.getSimpleName, e.getCause.getMessage)
      case e: Throwable          => new UserSqlException(e.getClass.getSimpleName, e.getMessage)
    }
  }
}
