package org.grenki.gsql
package context

import engine.Engine

import scala.collection.mutable.{Map => MutMap}

class Context[Type] {
  val engine = new Engine()
  val vars: MutMap[String, Type] = MutMap[String, Type]()

  def setVar(key: String, value: Type): Unit =
    vars.put(key, value)

  def getVar(key: String): Type =
    vars(key)

  def execute(stmt: String) =
    engine.execute(stmt)
}
