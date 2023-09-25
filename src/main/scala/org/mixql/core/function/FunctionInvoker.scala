package org.mixql.core.function

import org.mixql.core.context.{EngineContext, Context}
import org.mixql.core.context.gtype._

import java.lang.reflect.Method
import scala.annotation.meta.param
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global

object FunctionInvoker {

  def invokeAsync(functions: Map[String, Any],
                  funcName: String,
                  _contexts: List[Object], // To support not only mixql-core context
                  args: List[Any] = Nil,
                  kwargs: Map[String, Object] = Map.empty): Future[Type] = {
    val contexts = _contexts.map(c => {
      if (c.isInstanceOf[Context])
        c.asInstanceOf[Context].fork()
      else
        c
    })
    Future[Type] {
      pack(invoke(functions, funcName, contexts, args, kwargs))
    }
  }

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
        case Some(func) => invokeRegisteredFunc(func, funcName, contexts, args, kwargs)
        case None =>
          val ctxFiltered = contexts.filter(tuple => tuple.isInstanceOf[Context])
          if (ctxFiltered.nonEmpty) {
            val ctx = ctxFiltered.head.asInstanceOf[Context]
            val func = ctx.getVar(funcName)
            if (func.isInstanceOf[SqlLambda])
              invokeRegisteredFunc(func, funcName, contexts, args, kwargs)
            else if (ctx.currentEngine.getDefinedFunctions().contains(funcName.toLowerCase))
              unpack(
                ctx.currentEngine
                  .executeFunc(funcName, new EngineContext(ctx, ctx.currentEngineAllias), kwargs, args.map(pack): _*)
              )
            else
              throw new NoSuchMethodException(s"no function $funcName was founded to invoke")
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
        throw e
        throw new FunctionInvokerException(errorMsg)
      case e: Throwable => throw e
    }
  }

  private def invokeRegisteredFunc(func: Any,
                                   funcName: String,
                                   contexts: List[Object],
                                   args: List[Any] = Nil,
                                   kwargs: Map[String, Object] = Map.empty): Any = {
    func match {
      case l: List[_] =>
        for (f <- l) {
          val applyMethods = f.getClass.getMethods.filter(x =>
            // x.getParameters.length != 0 &&
            x.getParameters.exists(y => y.getType.getName != "java.lang.Object") &&
              x.getName == "apply"
          )

          if (compareFunctionTypes(applyMethods(0), args)) {
            return invokeFunc(f.asInstanceOf[Object], contexts, args.map(a => a.asInstanceOf[Object]), kwargs, funcName)
          }
        }
        throw new RuntimeException(s"Can't find function `$funcName` in $l [${l.length}] params=$args")
      case _ => invokeFunc(func.asInstanceOf[Object], contexts, args.map(a => a.asInstanceOf[Object]), kwargs, funcName)
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
        case "long"   => return right.getName == "java.lang.Long"
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
      return obj.asInstanceOf[SqlLambda].apply(contexts.head.asInstanceOf[Context], args: _*)
    val a = obj.getClass.getMethods.find(p =>
      p.getName == "apply" &&
        (p.getParameters.length == 0 || p.getParameters()(0).getName.toLowerCase != "v1")
    )
    a match {
      case None => throw new RuntimeException(s"Can't find method `apply` in function $funcName")
      case Some(apply) =>
        val applyParams = apply.getParameters
        val lb: ListBuffer[Object] = ListBuffer()
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
            // cast long to int if necessary
            val funcParamName = applyParams(i - 1).getType.getName
            if ((funcParamName == "int" || funcParamName == "java.lang.Integer") && args1.head.isInstanceOf[Long])
              lb += args1.head.asInstanceOf[Long].toInt.asInstanceOf[Object]
            else
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
        var res: Any = null
        try {
          res = apply.invoke(obj, lb.toArray: _*)
        } catch {
          case e: java.lang.reflect.InvocationTargetException =>
            if (e.getTargetException != null && e.getTargetException.getMessage
                  .contains("class java.lang.Long cannot be cast")) {
              val longs = lb.last
              lb.remove(lb.length - 1)
              lb += longs.asInstanceOf[Seq[Long]].map(_.toInt)
              res = apply.invoke(obj, lb.toArray: _*)
            } else
              throw e
        }
        res
    }
  }

  private def getDefParamsFor(obj: Object, i: Int): Object = {
    val paramName = s"apply$$default$$$i"
    val defval = obj.getClass.getMethod(paramName)
    defval.invoke(obj)
  }
}
