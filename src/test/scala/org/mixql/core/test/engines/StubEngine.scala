package org.mixql.core.test.engines

import org.mixql.core.context.ContextVars

import scala.collection.mutable.{Queue, Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.context.gtype._
import org.mixql.core.context.gtype.implicitGtypeConversions._

class StubEngine extends Engine {
  val queue = new Queue[String]()

  override def name: String = "stub"

  override def execute(stmt: String, ctx: ContextVars): Type = {
    queue += stmt
    new Null()
  }

  val changedParams: MutMap[String, Type] = MutMap()

  override def executeFunc(name: String, ctx: ContextVars, params: Type*): Type = {
    name match {
      case "getnum" => 42
      case "getstr" => "42"
      case _ => throw new NoSuchMethodException(s"unknown func $name")
    }
  }

  override def paramChanged(name: String, ctx: ContextVars): Unit = {
    changedParams.put(name, ctx.getVar(name))
  }

  def getChangedParam(name: String): Type = {
    changedParams(name)
  }

  override def getDefinedFunctions(): List[String] = List("getnum", "getstr")
}
