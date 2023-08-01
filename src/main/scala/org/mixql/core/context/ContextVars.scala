package org.mixql.core.context

import org.mixql.core.context.gtype.Type

import scala.collection.mutable

class ContextVars(val context: Context) {
  def setVar(key: String, value: Type): Unit = context.setVar(key, value)

  def getVar(key: String): Type = context.getVar(key)

  def getVars(keys: List[String]): mutable.Map[String, Type] = {
    val res: mutable.Map[String, Type] = mutable.Map()
    keys.foreach(key => res.put(key, context.getVar(key)))
    res
  }

  def setVars(vars: mutable.Map[String, Type]): Unit = {
    vars.foreach(variable => context.setVar(variable._1, variable._2))
  }

  def setVars(vars: Map[String, Type]): Unit = {
    vars.foreach(variable => context.setVar(variable._1, variable._2))
  }

  def getVarsNames(): List[String] = {
    context.getScope().flatMap(scope => scope.keys.toList)
  }
}
