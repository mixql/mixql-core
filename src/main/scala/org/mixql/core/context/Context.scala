package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.function.{ArrayFunction, StringFunction}
import org.mixql.core.context.gtype._
import org.mixql.core
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import com.typesafe.config.ConfigValueType._
import org.mixql.core.logger.logDebug

import java.{util => ju}
import scala.reflect.ClassTag
import scala.collection.JavaConverters._

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
    "trim" -> StringFunction.trim
  ).map(t => t._1.toLowerCase -> t._2)
}

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
class Context(
  val engines: MutMap[String, Engine],
  defaultEngine: String,
  functionsInit: MutMap[String, Any] = MutMap[String, Any](),
  variablesInit: MutMap[String, Type] = MutMap[String, Type]()
) extends java.lang.AutoCloseable {

  import Context.defaultFunctions

  var functions: MutMap[String, Any] = defaultFunctions ++ functionsInit

  var currentEngine: Engine = null

  /** current engine name
    *
    * @return
    *   mixql.execution.engine param value
    */
  def currentEngineAllias: String = getVar("mixql.execution.engine").toString

  /** mixql.error.skip
    *
    * @return
    *   mixql.error.skip param value
    */
  def errorSkip: Boolean = getVar("mixql.error.skip") match {
    case t: bool => t.getValue
    case _: Type =>
      throw new Exception("something wrong mixql.error.skip must be bool")
  }

  /** mixql.engine.variables.update: one of "all", "current", "none"
    *
    * @return
    *   mixql.engine.variables.update param value
    */
  def engineVariablesUpdate: String = getVar(
    "mixql.engine.variables.update"
  ).toString

  /** add variable scope
    */
  def push_scope(): Unit = {
    scope = MutMap[String, Type]() :: scope
  }

  /** remove top variable scope
    */
  def pop_scope(): Unit = {
    val head = scope.head
    scope = scope.tail
    engineVariablesUpdate match {
      case "all" =>
        head.foreach(v =>
          engines.foreach(e => e._2.setParam(v._1, getVar(v._1)))
        )
      case "current" =>
        head.foreach(v => currentEngine.setParam(v._1, getVar(v._1)))
      case _ =>
    }
    setVar("mixql.execution.engine", getVar("mixql.execution.engine"))
  }

  /** Set current engine by name that registered in this context. Throws
    * [[java.util.NoSuchElementException]] if no engine with this name
    *
    * @param name
    *   of engine
    */
  def setCurrentEngine(name: String): Unit = {
    if (name == "interpolator")
      throw new IllegalArgumentException(
        "interpolator could not be set as current engine"
      )
    currentEngine = engines.get(name) match {
      case Some(value) =>
        scope.head.put("mixql.execution.engine", new string(name))
        value
      case None =>
        throw new NoSuchElementException(s"no engine with name $name")
    }
    engineVariablesUpdate match {
      case "all" =>
        engines.foreach(e =>
          e._2.setParam("mixql.execution.engine", new string(name))
        )
      case "current" =>
        currentEngine.setParam("mixql.execution.engine", new string(name))
      case _ =>
    }
  }

  /** register engine by this name. If there was other engine with this name it
    * would be removed from context
    *
    * @param engine
    *   to register
    */
  def addEngine(engine: Engine): Unit = {
    if (engine.name == "interpolator")
      throw new IllegalArgumentException(
        "engine cannot be registered as interpolator"
      )
    engines.put(engine.name, engine)
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
    if (name == "interpolator")
      throw new IllegalArgumentException(
        "engine cannot be registered as interpolator"
      )
    engines.put(name, engine)
  }

  /** get execution engine by name
    *
    * @param name
    *   of execution engine
    * @return
    *   engine
    */
  def getEngine(name: String): Option[Engine] =
    engines.get(name)

  /** get exectution engine by class
    *
    * @return
    *   the first engine isInstanceOf[T]
    */
  def getEngine[T <: Engine: ClassTag]: Option[T] = {
    val res = engines.values.flatMap {
      case e: T => Some(e)
      case _    => None
    }
    if (res.isEmpty)
      None
    else
      Some(res.head)
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
      currentEngine.execute(stmt)
    else
      currentEngine.getCursor(stmt)
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
  def execute(stmt: String, engine: String, expect_cursor: Boolean): Type =
    getEngine(engine) match {
      case Some(value) =>
        if (!expect_cursor) value.execute(stmt)
        else
          value.getCursor(stmt)
      case None => throw new NoSuchElementException(s"unknown engine $engine")
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
  def execute(
    stmt: String,
    engine: String,
    params: Map[String, Type],
    expect_cursor: Boolean
  ): Type =
    getEngine(engine) match {
      case Some(eng) =>
        val old = params.keys.map(name => name -> eng.getParam(name)).toMap
        params.foreach(p => eng.setParam(p._1, p._2))
        val res =
          if (!expect_cursor)
            eng.execute(stmt)
          else
            eng.getCursor(stmt)
        old.foreach(p => eng.setParam(p._1, p._2))
        res
      case None => throw new NoSuchElementException(s"unknown engine $engine")
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
    // set mixql param
    key match {
      case "mixql.execution.engine" =>
        setCurrentEngine(value.toString)
        return // WARN as deprecated
      case "mixql.error.skip" =>
        value match {
          case _: bool =>
          case _ =>
            throw new IllegalArgumentException("mixql.error.skip must be bool")
        }
      case "mixql.engine.variables.update" =>
        if (!(Set("all", "current", "none") contains value.toString))
          throw new IllegalArgumentException(
            "mixql.engine.variables.update must be one of: all, current, none"
          )
      case _ =>
    }
    // set variable value
    value match {
      case _: Null =>
        scope.head.remove(key)
      case _ =>
        scope.head.put(key, value)
    }
    engineVariablesUpdate match {
      case "all"     => engines.foreach(e => e._2.setParam(key, value))
      case "current" => currentEngine.setParam(key, value)
      case _         =>
    }
  }

  /** get the variable value by name
    *
    * @param key
    *   variable name
    * @return
    *   variable value
    */
  def getVar(key: String): Type = {
    scope.foreach(vars => {
      val res = vars.getOrElse(key, new Null())
      res match {
        case _: Null =>
        case other   => return other
      }
    })
    new Null()
  }

  /** interpolate statement via current context
    *
    * @param stmt
    *   statement to interpolate
    * @return
    *   interpolated statement
    */
  def interpolate(stmt: String): String = {
    interpolator.currentEngine = currentEngine
    currentEngine = interpolator
    core.run(stmt + ";", this)
    setCurrentEngine(currentEngineAllias)
    interpolator.interpolated
  }

  /** register function with passed name. If there was other function with this
    * name it would be removed from context
    *
    * @param name
    *   of funtion
    * @param function
    */
  def addFunction(name: String, function: Any): Unit = {
    if (functions.contains(name.toLowerCase()))
      throw new InstantiationException(s"function $name is already defined")
    else {
      logDebug(
        "Functions map after adding function: " + functions.keySet.toList.sorted
          .toString()
      )
      val map = Map(name.toLowerCase() -> function)
      functions = functions ++ map;
      logDebug(
        "Functions map after adding function: " + functions.keySet.toList.sorted
          .toString()
      )
    }
  }

  override def close(): Unit = {
    logDebug("mixql core context: starting close")
    logDebug(
      "mixql core context: stop engines, if they were not closed before by shutdown command"
    )
    engines.values.foreach(engine => {
      import java.lang.AutoCloseable
      if (engine.isInstanceOf[AutoCloseable]) {
        val engineCloseable: AutoCloseable = engine.asInstanceOf[AutoCloseable]
        logDebug(s"mixql core context: stopping engine " + engine.name)
        engineCloseable.close()
      }
    })
  }

  def getScope(): List[Map[String, Type]] = {
    for { w <- scope } yield Map(w.toSeq: _*)
  }

  private var scope: List[MutMap[String, Type]] = null
  private val interpolator = new Interpolator()

  _init_(defaultEngine, variablesInit)

  private def _init_(
    defaultEngine: String,
    variables: MutMap[String, Type] = MutMap[String, Type]()
  ) = {
    val config = ConfigFactory.load()

    val mixqlParams = initMixqlParams(config, defaultEngine)
    if (config.hasPath("mixql.variables.init")) {
      val confVars = recurseParseParams(
        config.getObject("mixql.variables.init")
      )
      mixqlParams ++= confVars
    }
    mixqlParams ++= variables
    scope = List[MutMap[String, Type]](mixqlParams)

    currentEngine = engines(defaultEngine)
    engineVariablesUpdate match {
      case "all" =>
        scope.head.foreach(v => engines.foreach(e => e._2.setParam(v._1, v._2)))
      case "current" =>
        scope.head.foreach(v => currentEngine.setParam(v._1, v._2))
      case _ =>
    }
  }

  private def initMixqlParams(
    config: Config,
    defaultEngine: String
  ): MutMap[String, Type] = {
    val result: MutMap[String, Type] = MutMap[String, Type]()

    val errorSkip = config.getBoolean("mixql.error.skip")
    result += "mixql.error.skip" -> new bool(errorSkip)

    val evu = config.getString("mixql.engine.variables.update")
    if (!(Set("all", "current", "none") contains evu))
      throw new IllegalArgumentException(
        "mixql.engine.variables.update must be one of: all, current, none"
      )
    result += "mixql.engine.variables.update" -> new string(evu)

    val currentEngineAllias =
      if (config.hasPath("mixql.execution.engine"))
        config.getString("mixql.execution.engine")
      else
        defaultEngine
    result += "mixql.execution.engine" -> new string(currentEngineAllias)

    result
  }

  private def recurseParseParams(
    params: ConfigObject,
    scope: String = ""
  ): MutMap[String, Type] = {
    val res = MutMap[String, Type]()
    params.asScala.foreach(kv => {
      if (kv._2.valueType() == OBJECT)
        res ++= recurseParseParams(
          kv._2.asInstanceOf[ConfigObject],
          scope + kv._1 + "."
        )
      else
        res += scope + kv._1 -> convertConfigValue(kv._2.unwrapped)
    })
    res
  }

  private def convertConfigValue(value: Object): Type = {
    if (value == null)
      new Null()
    else if (value.isInstanceOf[Boolean])
      new bool(value.asInstanceOf[Boolean])
    else if (value.isInstanceOf[String])
      new string(value.asInstanceOf[String])
    else if (value.isInstanceOf[Integer])
      new gInt(value.asInstanceOf[Integer])
    else if (value.isInstanceOf[Double])
      new gDouble(value.asInstanceOf[Double])
    else if (value.isInstanceOf[ju.List[Object]]) {
      new array(
        value
          .asInstanceOf[ju.List[Object]]
          .asScala
          .map(convertConfigValue)
          .toArray
      )
    } else throw new Exception("unknown param type")
  }

  private sealed class Interpolator extends Engine {

    var interpolated: String = ""

    override def name: String = ""

    var currentEngine: Engine = null

    override def execute(stmt: String): Type = {
      interpolated = stmt
      new Null()
    }

    override def executeFunc(name: String, params: Type*) =
      throw new UnsupportedOperationException(
        "interpolator dont have specific funcs"
      )

    override def setParam(name: String, value: Type): Unit = {}

    override def getParam(name: String): Type = currentEngine.getParam(name)

    override def isParam(name: String): Boolean = true
  }
}
