package org.mixql.core.context

import org.mixql.core.context.gtype.Type

class ContextVars(val context: Context){
  def setVar(key: String, value: Type): Unit = context.setVar(key, value)
  def getVar(key: String): Type = context.getVar(key)

}
