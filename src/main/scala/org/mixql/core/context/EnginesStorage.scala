package org.mixql.core.context

import scala.reflect.ClassTag
import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.engine.Engine
import org.mixql.core.context.gtype._

object EnginesStorage {

  def apply(engines: MutMap[String, Engine] = MutMap[String, Engine]()): EnginesStorage = {
    val params_storage = MutMap[String, VariablesStorage]()
    engines.foreach(e => params_storage += e._1 -> new VariablesStorage())
    new EnginesStorage(engines, params_storage)
  }
}

final class EnginesStorage(e: MutMap[String, Engine] = MutMap[String, Engine](),
                           p: MutMap[String, VariablesStorage] = MutMap[String, VariablesStorage](),
                           d: Int = 0) {

  def pushScope(): Unit = {
    enginesParams.foreach(_._2.pushScope())
    depth += 1
  }

  def popScope(): Unit = {
    enginesParams.foreach(_._2.popScope())
    depth -= 1
  }

  def fork(): EnginesStorage = {
    new EnginesStorage(engines, enginesParams.map(x => x._1 -> x._2.fork()), depth + 1)
  }

  def getEngine(name: String): Option[Engine] = {
    engines.get(name)
  }

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

  def addEngine(name: String, engine: Engine): Unit = {
    engines += name -> engine
    enginesParams += name -> prepareNewStorage()
  }

  def addEngine(engine: Engine): Unit = {
    engines += engine.name -> engine
    enginesParams += engine.name -> prepareNewStorage()
  }

  def setEngineParam(engineName: String, name: String, value: Type): Unit = {
    if (!engines.contains(engineName))
      throw new NoSuchElementException(s"no engine with name: $engineName")
    enginesParams(engineName).setVar(name, value)
  }

  def setEngineGlobalParam(engineName: String, name: String, value: Type): Unit = {
    if (!engines.contains(engineName))
      throw new NoSuchElementException(s"no engine with name: $engineName")
    enginesParams(engineName).setGlobalVar(name, value)
  }

  def getEngineParam(engineName: String, name: String): Type = {
    if (!engines.contains(engineName))
      throw new NoSuchElementException(s"no engine with name: $engineName")
    enginesParams(engineName).getVar(name)
  }

  def getEngineParams(engineName: String): MutMap[String, Type] = {
    if (!engines.contains(engineName))
      throw new NoSuchElementException(s"no engine with name: $engineName")
    enginesParams(engineName).collect()
  }

  def close() = {
    engines.values.foreach(engine => {
      import java.lang.AutoCloseable
      if (engine.isInstanceOf[AutoCloseable]) {
        val engineCloseable: AutoCloseable = engine.asInstanceOf[AutoCloseable]
        engineCloseable.close()
      }
    })
  }

  private def prepareNewStorage() = {
    var d = depth
    val paramStorage = new VariablesStorage()
    while (d > 0) {
      paramStorage.pushScope()
      d -= 1
    }
    paramStorage
  }

  private val engines: MutMap[String, Engine] = e
  def engineNames = engines.keys.toList

  private val enginesParams: MutMap[String, VariablesStorage] = p

  private var depth: Int = d
}
