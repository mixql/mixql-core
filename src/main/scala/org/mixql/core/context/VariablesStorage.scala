package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.context.gtype._
import org.mixql.core.context.ConfigHelp.parseConfig
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import com.typesafe.config.ConfigValueType._

object VariablesStorage {

  def apply(defaultEngine: String, variablesInit: MutMap[String, Type] = MutMap[String, Type]()): VariablesStorage = {
    new VariablesStorage(initVariables(defaultEngine, variablesInit))
  }

  private def initVariables(defaultEngine: String, variablesInit: MutMap[String, Type]): MutMap[String, Type] = {
    val config = ConfigFactory.load()
    val mixqlParams = initMixqlParams(config, defaultEngine)
    if (config.hasPath("mixql.variables.init")) {
      val confVars = parseConfig(config.getObject("mixql.variables.init"))
      mixqlParams ++= confVars
    }
    mixqlParams ++= variablesInit
    mixqlParams
  }

  private def initMixqlParams(config: Config, defaultEngine: String): MutMap[String, Type] = {
    val result: MutMap[String, Type] = MutMap[String, Type]()

    val errorSkip = config.getBoolean("mixql.error.skip")
    result += "mixql.error.skip" -> new bool(errorSkip)

    // val currentEngineAllias =
    //   if (config.hasPath("mixql.execution.engine"))
    //     config.getString("mixql.execution.engine")
    //   else
    //     defaultEngine
    result += "mixql.execution.engine" -> new string(defaultEngine)

    result
  }

  private class Node(p: Node = null, variablesInit: MutMap[String, Type] = MutMap[String, Type]()) {
    val parent: Node = p

    val root: Node =
      if (p != null)
        p.root
      else
        this
    val scope: MutMap[String, Type] = variablesInit
  }
}

final class VariablesStorage(variablesInit: MutMap[String, Type] = MutMap[String, Type]()) {

  def pushScope(): Unit = {
    current = new VariablesStorage.Node(current)
  }

  def popScope(): Unit = {
    if (current == current.root)
      throw new IndexOutOfBoundsException("could not pop root scope")
    current = current.parent
  }

  def fork(): VariablesStorage = {
    val scope = new VariablesStorage.Node(current)
    val result = new VariablesStorage()
    result.current = scope
    result
  }

  def getVar(name: String): Type = {
    var curr_storage = current
    var result = curr_storage.scope.get(name)
    while (result.isEmpty && curr_storage.parent != null) {
      curr_storage = curr_storage.parent
      result = curr_storage.scope.get(name)
    }
    result.getOrElse(new none())
  }

  def setVar(name: String, value: Type): Unit = {
    value match {
      case _: none => current.scope.remove(name)
      case _       => current.scope.put(name, value)
    }
  }

  def setGlobalVar(name: String, value: Type): Unit = {
    value match {
      case _: none => current.root.scope.remove(name)
      case _       => current.root.scope.put(name, value)
    }
  }

  def collect(): MutMap[String, Type] = {
    var curr_storage = current
    var storage = List[MutMap[String, Type]](curr_storage.scope)
    while (curr_storage.parent != null) {
      curr_storage = curr_storage.parent
      storage = curr_storage.scope :: storage
    }
    var res: MutMap[String, Type] = MutMap[String, Type]()
    storage.foreach(scope => res ++= scope)
    res
  }

  private var current = new VariablesStorage.Node(null, variablesInit)
}
