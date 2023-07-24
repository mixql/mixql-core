package org.mixql.core.test.engines

import scala.collection.mutable.{Queue, Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.context.gtype._
import org.mixql.core.context.gtype.implicitGtypeConversions._

class StubEngine extends Engine {
  val queue = new Queue[String]()
  val param: MutMap[String, Type] = MutMap()
  override def name: String = "stub"
  override def execute(stmt: String): Type = {
    queue += stmt
    new Null()
  }

  override def executeFunc(name: String, params: Type*): Type = {
    name match {
      case "getnum" => 42
      case "getstr" => "42"
      case _        => throw new NoSuchMethodException(s"unknown func $name")
    }
  }
  override def setParam(name: String, value: Type): Unit = {
    param.put(name, value)
  }

  override def getParam(name: String): Type = {
    param.getOrElse(name, new Null())
  }

  override def getDefinedFunctions: List[String] = List("getnum", "getstr")
}
