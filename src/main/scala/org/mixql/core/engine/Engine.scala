package org.mixql.core.engine

import org.mixql.core.context.ContextVars
import org.mixql.core.context.gtype._

/** abstract class for execution engine
 */
abstract class Engine {

  /** engine name
   *
   * @return
   * name of engine
   */
  def name: String

  /** execute statement
   *
   * @param stmt
   * statement to execute
   * @return
   * the result of exection
   */
  def execute(stmt: String, ctx: ContextVars): Type

  /** execute statement
   *
   * @param stmt
   * statement to execute
   * @return
   * the result of exection as cursor
   */
  def getCursor(stmt: String, ctx: ContextVars): cursor = {
    import org.mixql.core.logger
    logger.logWarn("getCursor was not defined in engine " +
      name + ". Use execute method instead"
    )
    new gcursor(execute(stmt, ctx))
  }

  /** execute engine specific user function
   *
   * @param name
   * function name
   * @param params
   * function params
   * @return
   */
  def executeFunc(name: String, ctx: ContextVars, params: Type*): Type

  /** set param for engine
   *
   * @param name
   * of the param
   * @param value
   * of the param
   */
  def paramChanged(name: String, ctx: ContextVars): Unit

  /** get list of defined functions names in lower case
   *
   * @return
   * list of defined functions names in lower case
   */
  def getDefinedFunctions(ctx: ContextVars): List[String] = Nil
}
