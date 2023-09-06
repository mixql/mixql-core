package org.mixql.core.test.engines

import org.mixql.core.context.EngineContext

import scala.collection.mutable.{Queue, Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.context.gtype._
import org.mixql.core.context.gtype.implicitGtypeConversions._

class StubEngine extends Engine {
  val queue = new Queue[String]()

  override def name: String = "stub"

  override def executeImpl(stmt: String, ctx: EngineContext): Type = {
    queue += stmt
    new Null()
  }

  override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: Type*): Type = {
    if (kwargs.nonEmpty)
      throw new UnsupportedOperationException("named arguments are not supported in functions in engine " + name)

    name match {
      case "getnum" => 42
      case "getstr" => "42"
      case _        => throw new NoSuchMethodException(s"unknown func $name")
    }
  }

  override def getDefinedFunctions(): List[String] = List("getnum", "getstr")
}
