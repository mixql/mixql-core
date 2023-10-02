package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.function.{ArrayFunction, StringFunction, UtilFunction}
import org.mixql.core.context.gtype._
import org.mixql.core.logger.logDebug
import org.mixql.core
import org.mixql.core.context.ConfigHelp.parseConfig
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}

import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import scala.annotation.meta.param

object Context {

  val defaultFunctions: MutMap[String, Any] = MutMap[String, Any](
    "ascii" -> StringFunction.ascii,
    "base64" -> StringFunction.base64,
    "concat" -> StringFunction.concat,
    "concat_ws" -> StringFunction.concat_ws,
    "length" -> StringFunction.length,
    "substr" -> StringFunction.substr,
    "format_number" -> StringFunction.formatNumber,
    "size" -> ArrayFunction.size,
    "sort" -> ArrayFunction.sort,
    "startsWith" -> StringFunction.startsWith,
    "endsWith" -> StringFunction.endsWith,
    "isEmpty" -> StringFunction.isEmpty,
    "nonEmpty" -> StringFunction.nonEmpty,
    "findFirstIn" -> StringFunction.findFirstIn,
    "findAllIn" -> StringFunction.findAllIn,
    "replaceAllIn" -> StringFunction.replaceAllIn,
    "replaceFirstIn" -> StringFunction.replaceFirstIn,
    "split" -> StringFunction.split,
    "toLowerCase" -> StringFunction.toLowerCase,
    "toUpperCase" -> StringFunction.toUpperCase,
    "trim" -> StringFunction.trim,
    "is_error" -> UtilFunction.is_error,
    "await" -> UtilFunction.await
  ).map(t => t._1.toLowerCase -> t._2)

  /** the entry point to gsql api. Context stores registered engines, variables
    * and functions
    *
    * @param engines
    *   map engineName -> engine
    * @param defaultEngine
    *   name of current engine
    * @param function
    *   map functionName -> function
    * @param variables
    *   map variableName -> variableValue
    */
  def apply(engines: MutMap[String, Engine],
            defaultEngine: String,
            functionsInit: MutMap[String, Any] = MutMap[String, Any](),
            variablesInit: MutMap[String, Type] = MutMap[String, Type]()): Context = {
    val eng = EnginesStorage(engines += "interpolator" -> new Interpolator)
    val vars = VariablesStorage(initVariables(variablesInit))
    vars.setVar("mixql.execution.engine", new string(defaultEngine))
    new Context(eng, vars, defaultFunctions ++ functionsInit, true)
  }

  private def initVariables(variablesInit: MutMap[String, Type]): MutMap[String, Type] = {
    val config = ConfigFactory.load()
    val mixqlParams = initMixqlParams(config)
    if (config.hasPath("mixql.variables.init")) {
      val confVars = parseConfig(config.getObject("mixql.variables.init"))
      mixqlParams ++= confVars
    }
    mixqlParams ++= variablesInit
    mixqlParams
  }

  private def initMixqlParams(config: Config): MutMap[String, Type] = {
    val result: MutMap[String, Type] = MutMap[String, Type]()

    val errorSkip = config.getBoolean("mixql.error.skip")
    result += "mixql.error.skip" -> new bool(errorSkip)

    val currentEngineAllias = config.getString("mixql.execution.engine")
    result += "mixql.execution.engine" -> new string(currentEngineAllias)

    result
  }

  private sealed class Interpolator extends Engine {

    var interpolated: String = ""

    override def name: String = "interpolator"

    var currentEngine: Engine = null

    override def executeImpl(stmt: String, ctx: EngineContext): Type = {
      interpolated = stmt
      new string(interpolated)
    }

    override def executeFuncImpl(name: String, ctx: EngineContext, kwargs: Map[String, Object], params: Type*) =
      throw new UnsupportedOperationException("interpolator dont have specific funcs")
  }
}

class Context(eng: EnginesStorage,
              vars: VariablesStorage,
              func: MutMap[String, Any],
              private var isMainThread: Boolean = false)
    extends java.lang.AutoCloseable {

  /** current engine name
    *
    * @return
    *   mixql.execution.engine param value
    */
  def currentEngineAllias: String = getVar("mixql.execution.engine").toString

  /** current engine name
    *
    * @return
    *   current engine
    */
  def currentEngine: Engine = {
    getEngine(currentEngineAllias).get
  }

  /** mixql.error.skip
    *
    * @return
    *   mixql.error.skip param value
    */
  def errorSkip: Boolean =
    getVar("mixql.error.skip") match {
      case t: bool => t.getValue
      case _: Type => throw new Exception("something wrong mixql.error.skip must be bool")
    }

  /** add variable scope
    */
  def pushScope(): Unit = {
    engines.pushScope()
    variables.pushScope()
  }

  /** remove top variable scope
    */
  def popScope(): Unit = {
    engines.popScope()
    variables.popScope()
  }

  /** create new context that is leaf for this context. Like [[pushScope]] but
    * for async. New context know all vars and engines params for higher scopes
    */
  def fork(): Context = {
    new Context(engines.fork(), variables.fork(), functions)
  }

  /** Set current engine by name that registered in this context. Throws
    * [[java.util.NoSuchElementException]] if no engine with this name
    *
    * @param name
    *   of engine
    */
  def setCurrentEngine(name: String): Unit = {
    // check if engine with name exist
    getEngine(name) match {
      case None        => throw new NoSuchElementException(s"no engine with name: $name")
      case Some(value) =>
    }
    variables.setVar("mixql.execution.engine", new string(name))
  }

  /** Set current engine by name that registered in this context. Throws
    * [[java.util.NoSuchElementException]] if no engine with this name
    *
    * @param name
    *   of engine
    * @param params
    *   params to add to new current engine
    */
  def setCurrentEngine(name: String, params: MutMap[String, Type]): Unit = {
    // check if engine with name exist
    getEngine(name) match {
      case None        => throw new NoSuchElementException(s"no engine with name: $name")
      case Some(value) =>
    }
    params.foreach(p => engines.setEngineParam(name, p._1, p._2))
    variables.setVar("mixql.execution.engine", new string(name))
  }

  /** register engine by this name. If there was other engine with this name it
    * would be removed from context
    *
    * @param engine
    *   to register
    */
  def addEngine(engine: Engine): Unit = {
    engines.addEngine(engine)
  }

  /** register engine with passed name. name may differ to engine.name if there
    * was other engine with this name it would be removed from context
    *
    * @param name
    *   of engine to register. must not be "interpolator"
    * @param engine
    *   to register
    */
  def addEngine(name: String, engine: Engine): Unit = {
    engines.addEngine(name, engine)
  }

  /** get execution engine by name
    *
    * @param name
    *   of execution engine
    * @return
    *   engine
    */
  def getEngine(name: String): Option[Engine] = {
    engines.getEngine(name)
  }

  /** get exectution engine by class
    *
    * @return
    *   the first engine isInstanceOf[T]
    */
  def getEngine[T <: Engine: ClassTag]: Option[T] = {
    engines.getEngine[T]
  }

  /** execute statement on current engine
    *
    * @param stmt
    *   statement to execute
    * @return
    *   the result of execution
    */
  def execute(stmt: String, expect_cursor: Boolean): Type = {
    if (!expect_cursor)
      currentEngine.execute(stmt, new EngineContext(this, currentEngineAllias))
    else
      currentEngine.getCursor(stmt, new EngineContext(this, currentEngineAllias))
  }

  /** execute statement on engine
    *
    * @param stmt
    *   statement to execute
    * @param engine
    *   engine name to execute
    * @return
    *   the result of execution
    */
  def execute(stmt: String, engine: String, expect_cursor: Boolean): Type = {
    getEngine(engine) match {
      case Some(value) =>
        if (!expect_cursor)
          value.execute(stmt, new EngineContext(this, engine))
        else
          value.getCursor(stmt, new EngineContext(this, engine))
      case None => throw new NoSuchElementException(s"unknown engine $engine")
    }
  }

  /** execute statement on engine with specific params
    *
    * @param stmt
    *   statement to execute
    * @param engine
    *   engine name to execute
    * @param params
    *   params used to execute. old params sets after
    * @return
    *   the result of execution
    */
  def execute(stmt: String, engine: String, params: Map[String, Type], expect_cursor: Boolean): Type = {
    getEngine(engine) match {
      case Some(eng) =>
        // cache old params
        val old = params.map(param => param._1 -> engines.getEngineParam(engine, param._1))
        // set new params
        params.foreach(p => engines.setEngineParam(engine, p._1, p._2))
        val res =
          if (!expect_cursor)
            eng.execute(stmt, new EngineContext(this, engine))
          else
            eng.getCursor(stmt, new EngineContext(this, engine))
        // restore old params
        old.foreach(p => engines.setEngineParam(engine, p._1, p._2))
        res
      case None => throw new NoSuchElementException(s"unknown engine $engine")
    }
  }

  /** set variable value. if key starts with some engines name then this engines
    * param updates by value
    *
    * @param key
    *   the variable or engine param name
    * @param value
    *   the value of variable or param
    */
  def setVar(key: String, value: Type): Unit = {
    variables.setVar(key, value)
  }

  /** get the variable value by name
    *
    * @param key
    *   variable name
    * @return
    *   variable value
    */
  def getVar(key: String): Type = {
    variables.getVar(key)
  }

  /** get the param value by name for engine, if no param found var with this
    * name returns
    *
    * @param key
    *   param name
    * @param engineName
    *   engine name
    * @return
    *   param value
    */
  def getParam(key: String, engineName: String): Type = {
    engines.getEngineParam(engineName, key) match {
      case _: none => getVar(key)
      case other   => other
    }
  }

  /** get all vars for this scope + all params for current engine
    *
    * @return
    *   map: param name -> param value
    */
  def getParams(): MutMap[String, Type] = {
    variables.collect() ++ engines.getEngineParams(currentEngineAllias)
  }

  /** get all vars for this scope + all params for engineName engine
    *
    * @param engineName
    *   name of engine to get params
    * @return
    *   map: param name -> param value
    */
  def getParams(engineName: String): MutMap[String, Type] = {
    variables.collect() ++ engines.getEngineParams(engineName)
  }

  /** interpolate statement via current context
    *
    * @param stmt
    *   statement to interpolate
    * @return
    *   interpolated statement
    */
  def interpolate(stmt: String): String = {
    val prev = currentEngineAllias
    setCurrentEngine("interpolator")
    val res = core.run(stmt + ";", this)
    setCurrentEngine(prev)
    res.toString
  }

  /** register function with passed name. If there was other function with this
    * name it would be removed from context
    *
    * @param name
    *   of funtion
    * @param function
    */
  def addFunction(name: String, function: Any): Unit = {
    functions += name -> function
  }

  override def close(): Unit = {
    engines.close()
  }

  def getVars(): MutMap[String, Type] = {
    variables.collect()
  }

  private val variables: VariablesStorage = vars

  private val engines: EnginesStorage = eng

  def engineNames = engines.engineNames

  val functions: MutMap[String, Any] = func
}
