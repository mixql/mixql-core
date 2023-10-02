package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import org.mixql.core.context.mtype._

import com.typesafe.config.ConfigValueType._

object VariablesStorage {

  def apply(variablesInit: MutMap[String, MType] = MutMap[String, MType]()): VariablesStorage = {
    new VariablesStorage(variablesInit)
  }

  private class Node(p: Node = null, variablesInit: MutMap[String, MType] = MutMap[String, MType]()) {
    val parent: Node = p

    val root: Node =
      if (p != null)
        p.root
      else
        this
    val scope: MutMap[String, MType] = variablesInit
  }
}

final class VariablesStorage(variablesInit: MutMap[String, MType] = MutMap[String, MType]()) {

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

  def getVar(name: String): MType = {
    var curr_storage = current
    var result = curr_storage.scope.get(name)
    while (result.isEmpty && curr_storage.parent != null) {
      curr_storage = curr_storage.parent
      result = curr_storage.scope.get(name)
    }
    result.getOrElse(new MNone())
  }

  def setVar(name: String, value: MType): Unit = {
    value match {
      case _: MNone => current.scope.remove(name)
      case _        => current.scope.put(name, value)
    }
  }

  def setGlobalVar(name: String, value: MType): Unit = {
    value match {
      case _: MNone => current.root.scope.remove(name)
      case _        => current.root.scope.put(name, value)
    }
  }

  def collect(): MutMap[String, MType] = {
    var curr_storage = current
    var storage = List[MutMap[String, MType]](curr_storage.scope)
    while (curr_storage.parent != null) {
      curr_storage = curr_storage.parent
      storage = curr_storage.scope :: storage
    }
    var res: MutMap[String, MType] = MutMap[String, MType]()
    storage.foreach(scope => res ++= scope)
    res
  }

  private var current = new VariablesStorage.Node(null, variablesInit)
}
