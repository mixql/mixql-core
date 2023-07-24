package org.mixql.core.test.engines

import org.mixql.core.context.gtype
import org.mixql.core.context.gtype.{Type, array, gInt}
import org.mixql.core.engine.Engine

class CursorTestEngine1 extends Engine {
  var query: String = ""

  override def name: String = "CursorTestEngine1"

  val rand = new scala.util.Random

  override def execute(stmt: String): Type = {
    query = stmt
    new array(
      Array[Type](new gInt(rand.nextInt()),
                  new gInt(rand.nextInt()),
                  new gInt(rand.nextInt()),
                  new gInt(rand.nextInt()),
                  new gInt(rand.nextInt()),
                  new gInt(rand.nextInt()),
                  new gInt(rand.nextInt())))
  }

  override def executeFunc(name: String, params: Type*): Type = ???

  override def getParam(name: String): Type = new gtype.Null()

  override def setParam(name: String, value: Type): Unit = {}
}
