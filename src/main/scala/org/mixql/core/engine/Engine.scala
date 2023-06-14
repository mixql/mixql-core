package org.mixql.core.engine

import org.mixql.core.context.gtype._

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

  /** execute statement
   *
   * @param stmt
   * statement to execute
   * @return
   * the result of exection as cursor
   */
  def executeCursor(stmt: String): cursor = {
    import org.mixql.core.logger
    logger.logWarn("ExecuteCursor was not defined in engine " +
      name + ". Use execute method instead"
    )
    new gcursor(execute(stmt))
  }

  /** execute engine specific user function
    *
    * @param name
    *   function name
    * @param params
    *   function params
    * @return
    */
  def executeFunc(name: String, params: Type*): Type

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
    * @param name
    *   for the param
    * @return
    *   true if param, false if not
    */
  def isParam(name: String): Boolean = true

  /** get list of defined functions names in lower case
    *
    * @return
    *   list of defined functions names in lower case
    */
  def getDefinedFunctions: List[String] = Nil
}
