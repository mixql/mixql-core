package org.mixql.core.function

import org.mixql.core.context.Context
import org.mixql.core.exception.MException

import scala.concurrent.Future
import org.mixql.core.context.mtype.{MAsync, MNone}
import org.mixql.core.engine.Engine
import org.mixql.core.logger.{logInfo, logWarn}

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
