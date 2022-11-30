package org.mixql.core.test.stub

import scala.collection.mutable.{Queue, Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.context.gtype._

class StubEngine extends Engine {
  val queue = new Queue[String]()
  val param: MutMap[String, Type] = MutMap()
  override def name: String = "stub"
  override def execute(stmt: String): Type = {
    queue += stmt
    Null
  }

  override def setParam(name: String, value: Type): Unit = {
    param.put(name, value)
  }

  override def getParam(name: String): Type = {
    param.getOrElse(name, Null)
  }
}
