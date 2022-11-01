package org.grenki.gsql.context

import scala.collection.mutable.{Map => MutMap}
import org.grenki.gsql.engine.{Engine}
import org.grenki.gsql.function.StringFunction
import org.grenki.gsql.context.gtype._
import org.grenki.gsql

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
  val variables: MutMap[String, Type] = MutMap[String, Type](),
  val functions: MutMap[String, Any] = MutMap[String, Any](
    "ascii" -> StringFunction.ascii,
    "base64" -> StringFunction.base64,
    "concat" -> StringFunction.concat,
    "concat_ws" -> StringFunction.concat_ws,
    "length" -> StringFunction.length,
    "substr" -> StringFunction.substr
  )
) {

  private sealed class Interpolator extends Engine {

    var interpolated: String = ""
    override def name: String = ""

    var currentEngine: Engine = null

    override def execute(stmt: String): Type = {
      interpolated = stmt
      Null
    }

    override def setParam(name: String, value: Type): Unit = {}

    override def getParam(name: String): Type = currentEngine.getParam(name)

    override def isParam(name: String): Boolean = true
  }

  var currentEngine: Engine = engines(defaultEngine)
  var currentEngineAllias: String = defaultEngine

  private val interpolator = new Interpolator()

  /** Set current engine by name that registered in this context. Throws
    * [[java.util.NoSuchElementException]] if no engine with this name
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
    if (currentEngine.isParam(key))
      currentEngine.setParam(key, value)
    // set grenki param
    key match {
      case "grenki.execution.engine" => setCurrentEngine(value.toString)
      case _                         =>
    }
    // set variable value
    value match {
      case Null =>
        variables.remove(key)
      case _ =>
        variables.put(key, value)
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
    // TODO если текущий движок spark2, мы ставим параметр
    // затем переключаемся на spark3 и берем параметр то значение будет от spark2
    variables.getOrElse(key, Null) match {
      case Null  => currentEngine.getParam(key)
      case other => other
    }
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
    gsql.run(stmt + ";", this)
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
  def addFunction(name: String, function: Any): Unit =
    functions.put(name, function)
}
