package org.mixql.core.test.engines

import org.mixql.core.context.{ContextVars, gtype}
import org.mixql.core.context.gtype.{Type, array, gInt}
import org.mixql.core.engine.Engine


class CursorTestEngine1 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine1"

  val rand = new scala.util.Random

  override def execute(stmt: String, ctx: ContextVars): Type = {
    query = stmt
    new array(Array[Type](
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt()),
      new gInt(rand.nextInt())
    ))
  }

  override def executeFunc(name: String, ctx: ContextVars, params: Type*): Type = ???


  override def paramChanged(name: String, ctx: ContextVars): Unit = {}
}
