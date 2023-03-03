package org.mixql.core.function

import org.mixql.core.context.Context
import org.mixql.core.context.gtype._

import java.lang.reflect.Method
import scala.annotation.meta.param
import scala.collection.mutable.ListBuffer

object FunctionInvoker {
  def invoke(
    functions: Map[String, Any],
    funcName: String,
    context: Context,
    args: Seq[Any] = Nil,
    kwargs: Map[String, Object] = Map.empty
  ): Any = {
    functions.get(funcName.toLowerCase()) match {
      case Some(func) =>
        func match {
          case l: List[_] =>
            for (f <- l) {
              val applyMethods =
                f.getClass.getMethods.filter(x =>
                  // x.getParameters.length != 0 &&
                  x.getParameters.exists(y =>
                    y.getType.getName != "java.lang.Object"
                  ) && x.getName == "apply"
                )

              if (compareFunctionTypes(applyMethods(0), args)) {
                return invokeFunc(
                  f.asInstanceOf[Object],
                  context,
                  args.map(a => a.asInstanceOf[Object]),
                  kwargs,
                  funcName
                )
              }
            }
            throw new RuntimeException(
              s"Can't find function `$funcName` in $l [${l.length}] params=$args"
            )
          case _ =>
            invokeFunc(
              func.asInstanceOf[Object],
              context,
              args.map(a => a.asInstanceOf[Object]),
              kwargs,
              funcName
            )
        }
      case None =>
        if (kwargs.nonEmpty)
        throw new UnsupportedOperationException(
          "named args for engine function not supported"
        )
        if (
          context.currentEngine.getDefinedFunctions.contains(funcName.toLowerCase)
        )
          unpack(context.currentEngine.executeFunc(funcName, args.map(pack): _*))
        else {
          val engine = context.engines.find(eng =>
            eng._2.getDefinedFunctions.contains(funcName)
          )
          engine match {
            case Some(value) => unpack(value._2.executeFunc(funcName, args.map(pack): _*))
            case None =>
              throw new NoSuchMethodException(
                s"no function $funcName found for any engine"
              )
          }
        }
    }
  }

  private def compareFunctionTypes(a: Method, paramsSeq: Seq[_]): Boolean = {
    val params = paramsSeq.toArray
    if (a.getParameters.length != params.length) {
      if (
        a.getParameters.last.getType.isAssignableFrom(
          Class.forName("scala.collection.immutable.Seq")
        )
      ) return true
      else return false
    }

    var i = 0
    for (p <- a.getParameters) {
      if (!isTypeEquals(p.getType, params(i).getClass)) return false
      i += 1
    }

    true
  }

  private def isTypeEquals(left: Class[_], right: Class[_]): Boolean = {
    if (left.isPrimitive) {
      left.getName match {
        case "int" =>
          return right.getName == "java.lang.Integer"
        case "double" =>
          return right.getName == "java.lang.Double"
      }
    }
    right.isAssignableFrom(left)
  }

  private def invokeFunc(
    obj: Object,
    context: Context,
    args: Seq[Object] = Nil,
    kwargs: Map[String, Object] = Map.empty,
    funcName: String
  ): Any = {
    if (obj.isInstanceOf[SqlLambda])
      return obj.asInstanceOf[SqlLambda].apply(args: _*)
    val a = obj.getClass.getMethods.find(p =>
      p.getName == "apply" && (p.getParameters.length == 0 || p
        .getParameters()(0)
        .getName.toLowerCase != "v1")
    )
    a match {
      case Some(apply) =>
        val applyParams = apply.getParameters
        var lb: ListBuffer[Object] = ListBuffer()
        var args1 = args
        var kwargs1 = kwargs
        var i = 1
        val size = applyParams.length
        applyParams.foreach(param => {
          val pname = param.getName
          val ptype = param.getType
          val cc = "org.mixql.core.context.Context"
          val seqc = "scala.collection.immutable.Seq"
          val seqcOld = "scala.collection.Seq" //In case of scala 2.12
          // argument is variable number of args like gg: String*
          if (i == size && (ptype.getName == seqc) || (ptype.getName == seqcOld)) {
            lb += args1
          } else if (
            ptype.getName == cc ||
            ptype == Context.getClass
          ) {
            lb += context
          } else if (kwargs1.contains(pname)) {
            lb += kwargs1(pname)
            kwargs1 = kwargs1 - pname
          } else if (args1.nonEmpty) {
            lb += args1.head
            args1 = args1.tail
          } else {
            lb += getDefParamsFor(obj, i)
          }
          i += 1
        })
        apply.invoke(obj, lb.toArray: _*)
      case None =>
        throw new RuntimeException(
          s"Can't find method `apply` in function $funcName"
        )
    }
  }

  private def getDefParamsFor(obj: Object, i: Int): Object = {
    val paramName = s"apply$$default$$$i"
    val defval = obj.getClass.getMethod(paramName)
    defval.invoke(obj)
  }
}
