package org.grenki.gsql.engine

import org.grenki.gsql.context.gtype._

/** abstract class for execution engine
  */
abstract class Engine {

  /** engine name
    *
    * @return
    *   name of engine
    */
  def name: String

  /** execute statement
    *
    * @param stmt
    *   statement to execute
    * @return
    *   the result of exection
    */
  def execute(stmt: String): Type

  /** set param for engine
    *
    * @param name
    *   of the param
    * @param value
    *   of the param
    */
  def setParam(name: String, value: Type): Unit

  /** get engine param value
    *
    * @param name
    *   of the param
    * @return
    *   param value
    */
  def getParam(name: String): Type

  /** check if it is engine param
    *
    * @param name for the param
    * @return true if param, false if not
    */
  def isParam(name: String): Boolean = true
}
