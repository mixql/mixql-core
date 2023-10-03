package org.mixql.core.test.engines

import org.mixql.core.context.EngineContext
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine

class CursorTestEngine1 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine1"

  val rand = new scala.util.Random

  override def executeImpl(stmt: String, ctx: EngineContext): MType = {
    query = stmt
    new MArray(
      Array[MType](
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt()),
        new MInt(rand.nextInt())
      )
    )
  }

  override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: MType*): MType =
    ???
}
