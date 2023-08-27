package org.mixql.core.test.engines

import org.mixql.core.context.{EngineContext, gtype}
import org.mixql.core.context.gtype.{Type, array, gInt}
import org.mixql.core.engine.Engine

class CursorTestEngine1 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine1"

  val rand = new scala.util.Random

  override def executeImpl(stmt: String, ctx: EngineContext): Type = {
    query = stmt
    new array(
      Array[Type](
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt()),
        new gInt(rand.nextInt())
      )
    )
  }

  override def executeFuncImpl(name: String, ctx: EngineContext, params: Type*): Type = ???
}
