package org.mixql.core.function

import org.mixql.core.context.{EngineContext, Context}
import org.mixql.core.context.gtype._

import java.lang.reflect.Method
import scala.annotation.meta.param
import scala.collection.mutable.ListBuffer
import scala.util.Try

object FunctionInvoker {

  def invoke(functions: Map[String, Any],
             funcName: String,
             _contexts: List[Object], // To support not only mixql-core context
             args: List[Any] = Nil,
             kwargs: Map[String, Object] = Map.empty): Any = {
    val contexts =
      if (_contexts == null)
        List[Object]()
      else
        _contexts
    try {
      functions.map(t => t._1.toLowerCase -> t._2).get(funcName.toLowerCase()) match {
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
                    contexts,
                    args.map(a => a.asInstanceOf[Object]),
                    kwargs,
                    funcName
                  )
                }
              }
              throw new RuntimeException(s"Can't find function `$funcName` in $l [${l.length}] params=$args")
            case _ =>
              invokeFunc(func.asInstanceOf[Object], contexts, args.map(a => a.asInstanceOf[Object]), kwargs, funcName)
          }
        case None =>
          val ctxFiltered = contexts.filter(tuple => tuple.isInstanceOf[Context])
          if (ctxFiltered.nonEmpty) {
            val ctx = ctxFiltered.head.asInstanceOf[Context]
            if (ctx.currentEngine.getDefinedFunctions().contains(funcName.toLowerCase))
              unpack(ctx.currentEngine.executeFunc(funcName, new EngineContext(ctx), kwargs, args.map(pack): _*))
            else {
              val engine = ctx.engines.find(eng => {
                if (eng._2.name != ctx.currentEngine.name)
                  eng._2.getDefinedFunctions().contains(funcName)
                else
                  false
              })
              engine match {
                case Some(value) =>
                  unpack(value._2.executeFunc(funcName, new EngineContext(ctx), kwargs, args.map(pack): _*))
                case None => throw new NoSuchMethodException(s"no function $funcName found for any engine")
              }
            }
          } else {
            throw new NoSuchMethodException(s"no function $funcName was founded to invoke")
          }
      }
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        val errorMsg: String =
          "InvocationTargetException: \n" +
            e.getClass.getName + " msg : " + e.getMessage + "\n" +
            "target's exception: " + e.getTargetException.getClass.getName + "\n" +
            "target's exception msg: " + e.getTargetException.getMessage + "\n" +
            "target exception stacktrace: " + {
              import java.io.PrintWriter
              import java.io.StringWriter
              var sw: StringWriter = null
              var pw: PrintWriter = null
              try {
                sw = new StringWriter()
                pw = new PrintWriter(sw)
                e.getTargetException.printStackTrace(pw)
                sw.toString
              } finally {
                Try(
                  if (pw != null)
                    pw.close()
                )
                Try(
                  if (sw != null)
                    sw.close()
                )
              }
            }

        throw new FunctionInvokerException(errorMsg)
      case e: Throwable => throw e
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
                         contexts: List[Object],
                         args: Seq[Object] = Nil,
                         kwargs: Map[String, Object] = Map.empty,
                         funcName: String): Any = {
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
          var addedArg = false
          if (i == size && (ptype.getName == seqc) ||
              (ptype.getName == seqcOld) && !addedArg) {
            lb += args1
            addedArg = true
          }

          if (contexts.nonEmpty && !addedArg) {
            try {
              contexts.foreach(ctx =>
                if (ptype.getName == ctx.getClass.getName || ptype == ctx.getClass) {
                  lb += ctx
                  addedArg = true
                  throw new BrakeException
                }
              )
            } catch {
              case _: BrakeException =>
              case e: Exception      => throw e
            }
          }

          if (kwargs1.contains(pname) && !addedArg) {
            lb += kwargs1(pname)
            addedArg = true
            kwargs1 = kwargs1 - pname
          }

          if (args1.nonEmpty && !addedArg) {
            lb += args1.head
            addedArg = true
            args1 = args1.tail
          }

          if (!addedArg) {
            lb += getDefParamsFor(obj, i)
            addedArg = true
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
