package org.mixql.core.context.mtype

import scala.concurrent.Future
import java.util.concurrent.ExecutionException
import org.mixql.core.exception.MException
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import java.lang.reflect.InvocationTargetException

final class MAsync(val fut: Future[Any]) extends MType {

  def await(): MType = {
    try {
      pack(Await.result(fut, Duration.Inf))
    } catch {
      case e: MException => e
      case e: InvocationTargetException =>
        new MException(e.getCause.getClass.getSimpleName, e.getCause.getMessage)
      case e: ExecutionException => new MException(e.getCause.getClass.getSimpleName, e.getCause.getMessage)
      case e: Throwable          => new MException(e.getClass.getSimpleName, e.getMessage)
    }
  }
}
