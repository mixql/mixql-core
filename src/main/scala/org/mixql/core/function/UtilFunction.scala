package org.mixql.core.function

import java.util.concurrent.atomic.AtomicReference
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, ExecutionContext, Promise}
import ExecutionContext.Implicits.global

import org.mixql.core.context.mtype._
import org.mixql.core.context.Context
import org.mixql.core.exception.MException
import org.mixql.core.engine.Engine
import org.mixql.core.logger.{logInfo, logWarn}

object UtilFunction {

  def firstCompletedOfNotError[T](futures: TraversableOnce[Future[T]])(implicit
    executor: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    val firstCompleteHandler =
      new AtomicReference[Promise[T]](p) with (Try[T] => Unit) {
        override def apply(v1: Try[T]): Unit =
          v1 match {
            case Success(value) =>
              getAndSet(null) match {
                case null => ()
                case some => some tryComplete v1
              }
            case Failure(exception) =>
          }
      }
    futures foreach { _.onComplete(firstCompleteHandler)(executor) }
    p.future
  }

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

  val await_all =
    new (Seq[Future[Any]] => Any) {

      override def apply(value: Seq[Future[Any]]): Any = {
        value.map(f => new MAsync(f).await())
      }
    }

  val await_any =
    new (Seq[Future[Any]] => Any) {

      override def apply(value: Seq[Future[Any]]): Any = {
        val firstNotError = firstCompletedOfNotError(value)
        val firstWhenAllCompleted = Future[Any] {
          value.map(f => new MAsync(f).await()).head
        }
        new MAsync(Future.firstCompletedOf(Seq(firstNotError, firstWhenAllCompleted))).await()
      }
    }

  val closeEngine =
    new ((Context, String) => MNone) {

      override def apply(ctx: Context, engineName: String = ""): MNone = {
        logInfo("[close_engine] started")
        val engine: Engine =
          if (engineName.isEmpty)
            ctx.currentEngine
          else
            ctx.getEngine(engineName.trim).get

        if (engine.isInstanceOf[AutoCloseable]) {
          val closableEngine = engine.asInstanceOf[AutoCloseable]
          logInfo("[close_engine] trigger engine's " + engine.name + " close")
          closableEngine.close()
        } else
          logWarn("[close_engine] unsupported engine " + engine.name + ". It's not AutoCloseable. Ignore it")

        MNone.get()
      }
    }
}
