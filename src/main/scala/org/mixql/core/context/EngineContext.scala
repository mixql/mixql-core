package org.mixql.core.context

import org.mixql.core.context.gtype.{Type, pack}
import org.mixql.core.function.FunctionInvoker

import scala.collection.mutable

class EngineContext(val context: Context) {
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
    context.getParams().keys.toList
  }

  /** invoke function using context, can call also functions from other engines
    * and default context functions
    *
    * @param funcName
    *   name of function
    * @param args
    *   arguments for function
    * @param kwargs
    *   named arguments for function
    */
  def invokeFunction(funcName: String, args: List[Any] = Nil, kwargs: Map[String, Object] = Map.empty): Type = {
    try {
      val res = FunctionInvoker.invoke(Map(context.functions.toSeq: _*), funcName, context, args, kwargs)
      pack(res)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        val errorMsg =
          "Error while invoke of " + funcName + " \n" +
            "Error: " + e.getClass.getName + " " + "msg: \n" +
            e.getMessage + "\n" +
            "target exception: " + e.getTargetException.getClass.getName + "\n" +
            "target exception msg: \n" + e.getTargetException.getMessage + "\n" +
            "target exception stacktrace: \n" + e.getTargetException.printStackTrace()
        throw new Exception(errorMsg)
      case e: Exception => throw e
    }
  }
}
