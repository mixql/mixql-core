package org.mixql.core.engine

import org.mixql.core.context.EngineContext
import org.mixql.core.context.mtype._
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
  final def execute(stmt: String, ctx: EngineContext): MType = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by execute request")

    engineStarted = true

    executeImpl(stmt, ctx)
  }

  /** execute statement
    *
    * @param stmt
    *   statement to execute
    * @return
    *   the result of execution as cursor
    */
  final def getCursor(stmt: String, ctx: EngineContext): MCursorBase = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by execute request expecting cursor")

    engineStarted = true
    getCursorImpl(stmt, ctx)
  }

  /** execute engine specific user function
    *
    * @param name
    *   function name
    * @param params
    *   function params
    * @return
    */
  final def executeFunc(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: MType*): MType = {
    if (!engineStarted)
      logInfo(s"Engine $name was triggered by executeFunc request")
    engineStarted = true
    executeFuncImpl(name, ctx, kwargs, params: _*)
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

  def executeImpl(stmt: String, ctx: EngineContext): MType

  def getCursorImpl(stmt: String, ctx: EngineContext): MCursorBase = {
    import org.mixql.core.logger
    logger.logWarn(
      s"getCursor was not defined in engine $name" +
        name + ". Use execute method instead"
    )
    new MCursor(execute(stmt, ctx))
  }

  def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: MType*): MType

}
