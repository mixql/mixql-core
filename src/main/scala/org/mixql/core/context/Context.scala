package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.function.{ArrayFunction, StringFunction}
import org.mixql.core.context.gtype._
import org.mixql.core

import scala.reflect.ClassTag

/** the entry point to gsql api. Context stores registered engines, variables
  * and functions
  *
  * @param engines
  *   map engineName -> engine
  * @param defaultEngine
  *   name of current engine
  * @param variables
  *   map variableName -> variableValue
  * @param function
  *   map functionName -> function
  */
class Context(
  val engines: MutMap[String, Engine],
  defaultEngine: String,
  variables: MutMap[String, Type] = MutMap[String, Type](),
  val functions: MutMap[String, Any] = MutMap[String, Any](
    "ascii" -> StringFunction.ascii,
    "base64" -> StringFunction.base64,
    "concat" -> StringFunction.concat,
    "concat_ws" -> StringFunction.concat_ws,
    "length" -> StringFunction.length,
    "substr" -> StringFunction.substr,
    "format_number" -> StringFunction.formatNumber,
    "size" -> ArrayFunction.size,
    "sort" -> ArrayFunction.sort
  )
) extends java.lang.AutoCloseable {

  var scope = List[MutMap[String, Type]](variables)

  private sealed class Interpolator extends Engine {

    var interpolated: String = ""

    override def name: String = ""

    var currentEngine: Engine = null

    override def execute(stmt: String): Type = {
      interpolated = stmt
      Null
    }

    override def executeFunc(name: String, params: Type*) = 
      throw new UnsupportedOperationException("interpolator dont have specific funcs")
      
    override def setParam(name: String, value: Type): Unit = {}

    override def getParam(name: String): Type = currentEngine.getParam(name)

    override def isParam(name: String): Boolean = true
  }

  var currentEngine: Engine = engines(defaultEngine)
  var currentEngineAllias: String = defaultEngine
  var grenkiErrorSkip: Boolean = false

  private val interpolator = new Interpolator()

  /** add variable scope
    */
  def push_scope(): Unit = {
    scope = MutMap[String, Type]() :: scope
  }

  /** remove top variable scope
    */
  def pop_scope(): Unit = {
    scope = scope.tail
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
        variables.put("grenki.execution.engine", string(name))
        value
      case None =>
        throw new NoSuchElementException(s"no engine with name $name")
    }
    currentEngineAllias = name
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
  def execute(stmt: String): Type =
    currentEngine.execute(stmt)

  /** execute statement on engine
    *
    * @param stmt
    *   statement to execute
    * @param engine
    *   engine name to execute
    * @return
    *   the result of execution
    */
  def execute(stmt: String, engine: String): Type =
    getEngine(engine) match {
      case Some(value) => value.execute(stmt)
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
  def execute(stmt: String, engine: String, params: Map[String, Type]): Type =
    getEngine(engine) match {
      case Some(eng) =>
        val old = params.keys.map(name => name -> eng.getParam(name)).toMap
        params.foreach(p => eng.setParam(p._1, p._2))
        val res = eng.execute(stmt)
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
    // set current engine param
    // if (currentEngine.isParam(key))
    //  currentEngine.setParam(key, value)
    // set grenki param
    key match {
      case "grenki.execution.engine" =>
        setCurrentEngine(value.toString) // WARN as deprecated
      case "grenki.error.skip" =>
        value match {
          case bool(value) =>
            grenkiErrorSkip = value
          case _ =>
            throw new IllegalArgumentException("grenki.error.skip must be bool")
        }
      case _ =>
    }
    // set variable value
    value match {
      case Null =>
        scope.head.remove(key)
      case _ =>
        scope.head.put(key, value)
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
      val res = vars.getOrElse(key, Null)
      res match {
        case Null  =>
        case other => return other
      }
    })
    Null
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
    if (functions.contains(name))
      throw new InstantiationException(s"function $name is already defined")
    else
      functions.put(name, function)
  }

  override def close(): Unit = {
    println("mixql core context: starting close")
    println(
      "mixql core context: stop engines, if they were not closed before by shutdown command"
    )
    engines.values.foreach(engine => {
      import java.lang.AutoCloseable
      if (engine.isInstanceOf[AutoCloseable]) {
        val engineCloseable: AutoCloseable = engine.asInstanceOf[AutoCloseable]
        println(s"mixql core context: stopping engine " + engine.name)
        engineCloseable.close()
      }
    })
  }
}
