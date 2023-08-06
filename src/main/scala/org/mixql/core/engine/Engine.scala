package org.mixql.core.engine

import org.mixql.core.context.EngineContext
import org.mixql.core.context.gtype._
import org.mixql.core.logger.logInfo

/** abstract class for execution engine
  */
abstract class Engine {
  protected var engineStarted: Boolean = false

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
  def execute(stmt: String, ctx: EngineContext): Type

  final def _execute(stmt: String, ctx: EngineContext): Type = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by execute request")

    engineStarted = true

    execute(stmt, ctx)
  }

  final def _getCursor(stmt: String, ctx: EngineContext): cursor = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by execute request expecting cursor")

    engineStarted = true
    getCursor(stmt, ctx)
  }

  /** execute statement
    *
    * @param stmt
    *   statement to execute
    * @return
    *   the result of execution as cursor
    */
  def getCursor(stmt: String, ctx: EngineContext): cursor = {
    import org.mixql.core.logger
    logger.logWarn(
      s"getCursor was not defined in engine $name" +
        name + ". Use execute method instead"
    )
    new gcursor(_execute(stmt, ctx))
  }

  /** execute engine specific user function
    *
    * @param name
    *   function name
    * @param params
    *   function params
    * @return
    */
  def executeFunc(name: String, ctx: EngineContext, params: Type*): Type

  final def _executeFunc(name: String, ctx: EngineContext, params: Type*): Type = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by executeFunc request")
    engineStarted = true
    executeFunc(name, ctx, params: _*)
  }

  /** set param for engine
    *
    * @param name
    *   of the param
    * @param value
    *   of the param
    */
  def paramChanged(name: String, ctx: EngineContext): Unit

  final def _paramChanged(name: String, ctx: EngineContext): Unit = {
    if (engineStarted)
      paramChanged(name, ctx)
  }

  /** get list of defined functions names in lower case
    *
    * @return
    *   list of defined functions names in lower case
    */
  def getDefinedFunctions(): List[String] = {
    // Not to trigger engine by defined functions request
    // We will know what functions are defined, so can return just predefined list of functions names
    // otherwise we can add
    //    if (!engineStarted)
    //      logInfo(s" was triggered by getDefinedFunctions request")
    //    engineStarted = true
    Nil
  }
}
