package org.mixql.core.function

import org.mixql.core.context.{Context, EngineContext}
import org.mixql.core.context.gtype._

import java.lang.reflect.Method
import scala.annotation.meta.param
import scala.collection.mutable.ListBuffer
import scala.util.Try

object FunctionInvoker {

  def invoke(functions: Map[String, Any],
             funcName: String,
             context: Object, // To support not only mixql-core context
             args: List[Any] = Nil,
             kwargs: Map[String, Object] = Map.empty,
             cc: String = "org.mixql.core.context.Context" // To support not only mixql-core context
  ): Any = {
    functions.get(funcName.toLowerCase()) match {
      case Some(func) =>
        func match {
          case l: List[_] =>
            for (f <- l) {
              val applyMethods = f.getClass.getMethods.filter(x =>
                // x.getParameters.length != 0 &&
                x.getParameters.exists(y => y.getType.getName != "java.lang.Object") &&
                  x.getName == "apply"
              )

              if (compareFunctionTypes(applyMethods(0), args)) {
                return invokeFunc(
                  f.asInstanceOf[Object],
                  context,
                  args.map(a => a.asInstanceOf[Object]),
                  kwargs,
                  funcName,
                  cc
                )
              }
            }
            throw new RuntimeException(s"Can't find function `$funcName` in $l [${l.length}] params=$args")
          case _ =>
            invokeFunc(func.asInstanceOf[Object], context, args.map(a => a.asInstanceOf[Object]), kwargs, funcName, cc)
        }
      case None =>
        if (context.isInstanceOf[Context]) {
          val ctx = context.asInstanceOf[Context]
          if (kwargs.nonEmpty)
            throw new UnsupportedOperationException("named args for engine function not supported")
          if (ctx.currentEngine.getDefinedFunctions().contains(funcName.toLowerCase))
            unpack(ctx.currentEngine.executeFunc(funcName, new EngineContext(ctx), args.map(pack): _*))
          else {
            val engine = ctx.engines.find(eng => {
              if (eng._2.name != ctx.currentEngine.name)
                eng._2.getDefinedFunctions().contains(funcName)
              else
                false
            })
            engine match {
              case Some(value) => unpack(value._2.executeFunc(funcName, new EngineContext(ctx), args.map(pack): _*))
              case None        => throw new NoSuchMethodException(s"no function $funcName found for any engine")
            }
          }
        } else {
          throw new NoSuchMethodException(s"no function $funcName was founded to invoke")
        }
    }
  }

  private def compareFunctionTypes(a: Method, paramsSeq: Seq[_]): Boolean = {
    val params = paramsSeq.toArray
    if (a.getParameters.length != params.length) {
      if (a.getParameters.last.getType.isAssignableFrom({
            Try({ Class.forName("scala.collection.immutable.Seq") })
              .getOrElse({ Class.forName("scala.collection.Seq") })
          }))
        return true
      else
        return false
    }

    var i = 0
    for (p <- a.getParameters) {
      if (!isTypeEquals(p.getType, params(i).getClass))
        return false
      i += 1
    }

    true
  }

  private def isTypeEquals(left: Class[_], right: Class[_]): Boolean = {
    if (left.isPrimitive) {
      left.getName match {
        case "int"    => return right.getName == "java.lang.Integer"
        case "double" => return right.getName == "java.lang.Double"
      }
    }
    right.isAssignableFrom(left)
  }

  private def invokeFunc(obj: Object,
                         context: Object,
                         args: Seq[Object] = Nil,
                         kwargs: Map[String, Object] = Map.empty,
                         funcName: String,
                         cc: String): Any = {
    if (obj.isInstanceOf[SqlLambda])
      return obj.asInstanceOf[SqlLambda].apply(args: _*)
    val a = obj.getClass.getMethods.find(p =>
      p.getName == "apply" &&
        (p.getParameters.length == 0 || p.getParameters()(0).getName.toLowerCase != "v1")
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
          val seqc = "scala.collection.immutable.Seq"
          val seqcOld = "scala.collection.Seq" // In case of scala 2.12
          // argument is variable number of args like gg: String*
          if (i == size && (ptype.getName == seqc) ||
              (ptype.getName == seqcOld)) {
            lb += args1
          } else if (context != null &&
                     (ptype.getName == cc || ptype == context.getClass)) {
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
      case None => throw new RuntimeException(s"Can't find method `apply` in function $funcName")
    }
  }

  private def getDefParamsFor(obj: Object, i: Int): Object = {
    val paramName = s"apply$$default$$$i"
    val defval = obj.getClass.getMethod(paramName)
    defval.invoke(obj)
  }
}
