package org.grenki.gsql.function

import scala.collection.mutable.ListBuffer

object FunctionInvoker {
  def invoke(
    functions: Map[String, Any],
    funcName: String,
    params: Seq[Any] = Nil,
    paramsMap: Map[String, Object] = Map.empty
  ): Any = {
    def invokeFunc(
      obj: Any,
      params: Seq[Any] = Nil,
      paramsMap: Map[String, Object] = Map.empty
    ): Any = {
      val a = obj.getClass.getMethods.find(_.getName == "apply")
      a match {
        case Some(apply) =>
          val pc = apply.getParameters.length
          if (pc == params.length)
            apply.invoke(obj, params: _*)
          else {
            val lb = ListBuffer(params.toIndexedSeq: _*)
            for (i <- params.length + 1 to pc) {
              val paramName = apply.getParameters.apply(i - 1).getName
              lb += paramsMap.getOrElse(paramName, getDefParamsFor(obj, i))
            }
            if (lb.length > pc) {
              val head = lb.take(pc - 1)
              val tail = lb.drop(pc - 1)
              val bundle = head ++ Seq(tail.toSeq)
              apply.invoke(obj, bundle.toArray: _*)
            } else
              apply.invoke(obj, lb.toArray: _*)
          }
        case None =>
          throw new RuntimeException(
            s"Can't find method `apply` in function $funcName"
          )
      }
    }

    functions.get(funcName.toLowerCase()) match {
      case Some(func) => invokeFunc(func, params, paramsMap)
      case None =>
        throw new RuntimeException(s"Can't find function `$funcName`")
    }
  }

  private def getDefParamsFor(obj: Any, i: Int): Any = {
    val paramName = s"apply$$default$$$i"
    obj.getClass.getMethod(paramName).invoke(obj)
  }
}
