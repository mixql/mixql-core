package org.grenki.gsql.engine

import org.grenki.gsql.context.gtype._

/** abstract class for execution engine
  */
class Engine {

  /** engines name
    *
    * @return
    *   name of engine
    */
  def name: String = "stub"

  /** execute statement engine
    *
    * @param stmt
    *   statement to execute
    * @return
    *   the result of exection
    */
  def execute(stmt: String): Type = {
    println("execute: " + stmt)
    Null
  }

  /** set param for engine
    *
    * @param name
    *   of the param
    * @param value
    *   of the param
    */
  def setParam(name: String, value: Type): Unit = {}
}
