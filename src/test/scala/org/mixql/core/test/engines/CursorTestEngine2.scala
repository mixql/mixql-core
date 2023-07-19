package org.mixql.core.test.engines

import org.mixql.core.context.{ContextVars, gtype}
import org.mixql.core.context.gtype.{Type, bool, cursor, gInt, nothing}
import org.mixql.core.engine.Engine
import org.mixql.core.logger.logInfo

import scala.util.Random

class CursorTestEngine2 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine2"

  override def execute(stmt: String, ctx: ContextVars): Type = {
    query = stmt
    throw new Exception("execute was triggered instead of executeCursor")
  }

  override def getCursor(stmt: String, ctx: ContextVars): cursor = {
    query = stmt
    new CursorTest2(this, stmt: String)
  }

  override def executeFunc(name: String, ctx: ContextVars, params: Type*): Type = ???

  override def paramChanged(name: String, ctx: ContextVars): Unit = {}
}

class CursorTest2(engine: CursorTestEngine2, stmt: String) extends cursor {

  val countToFetch = 10;
  var currentCount = 0;

  var stream: Random = null

  override def close(): bool = {
    logInfo("close was triggered in CursorTest2 of engine " + engine.name)
    new bool(true)
  }

  override def open(): bool = {
    logInfo("open was triggered in CursorTest2 of engine " + engine.name)
    if (stream == null)
      stream = new scala.util.Random

    new bool(true)
  }

  override def fetch(): Type = {
    logInfo("fetch was triggered in CursorTest2 of engine " + engine.name)
    if (currentCount < countToFetch) {
      currentCount = currentCount + 1;
      new gInt(stream.nextInt())
    } else
      new nothing()
  }
}