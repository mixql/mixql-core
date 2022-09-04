package org.grenki.gsql.context

import org.grenki.gsql.engine.Engine

import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.context.gtype.Type

/** @param e map engine_name -> engine
  * 
  *
  */
class Context(e: MutMap[String, Engine] = MutMap[String, Engine]("stub" -> new Engine)) {
  val engine = e
  var currentEngine = engine("stub")
  val vars: MutMap[String, Type] = MutMap[String, Type]()

  def setVar(key: String, value: Type): Unit =
    vars.put(key, value)

  def getVar(key: String): Type =
    vars(key)

  def execute(stmt: String) =
    currentEngine.execute(stmt)
}
