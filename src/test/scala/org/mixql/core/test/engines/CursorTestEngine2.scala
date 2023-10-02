package org.mixql.core.test.engines

import org.mixql.core.context.EngineContext
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.logger.logInfo

import scala.util.Random

class CursorTestEngine2 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine2"

  override def executeImpl(stmt: String, ctx: EngineContext): MType = {
    query = stmt
    throw new Exception("execute was triggered instead of executeCursor")
  }

  override def getCursorImpl(stmt: String, ctx: EngineContext): cursor = {
    query = stmt
    new CursorTest2(this, stmt: String)
  }

  override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: MType*): MType =
    ???
}

class CursorTest2(engine: CursorTestEngine2, stmt: String) extends cursor {

  val countToFetch = 10;
  var currentCount = 0;

  var stream: Random = null

  override def close(): MBool = {
    logInfo("close was triggered in CursorTest2 of engine " + engine.name)
    new MBool(true)
  }

  override def open(): MBool = {
    logInfo("open was triggered in CursorTest2 of engine " + engine.name)
    if (stream == null)
      stream = new scala.util.Random

    new MBool(true)
  }

  override def fetch(): MType = {
    logInfo("fetch was triggered in CursorTest2 of engine " + engine.name)
    if (currentCount < countToFetch) {
      currentCount = currentCount + 1;
      new MInt(stream.nextInt())
    } else
      new MNone()
  }
}
