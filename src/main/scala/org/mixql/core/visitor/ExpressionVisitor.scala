package org.mixql.core.visitor

import org.mixql.core.context.mtype._
import org.mixql.core.function.MLambda
import org.mixql.core.function.FunctionInvoker
import org.antlr.v4.runtime.misc.Interval
import org.mixql.core.generated.sql

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import org.mixql.core.context.ControlContext
import org.mixql.core.context.mtype.MAsync

trait ExpressionVisitor extends BaseVisitor {

  def executeOther(stmt: String, engine: sql.Choose_engineContext): Try[MType] =
    Try {
      if (engine) {
        // execute on custom engine
        // get engine name
        val engineName =
          if (engine.expr)
            visit(engine.expr).toString
          else
            visit(engine.ident).toString
        if (engine.engine_params) {
          // execute with additional params
          val params =
            engine.engine_params.ident.asScala.map(visit(_).toString).zip(engine.engine_params.expr.asScala.map(visit))
              .toMap
          context.execute(stmt, engineName, params, false)
        } else {
          // execute with current params
          context.execute(stmt, engineName, false)
        }
      } else {
        // execute on current engine
        context.execute(stmt, false)
      }
    }

  override def visitVar(ctx: sql.VarContext): MType = {
    val res = context.getVar(visit(ctx.ident).toString)
    if (res.isInstanceOf[MString])
      new MString(res.toString)
    else
      res
  }

  override def visitExpr_concat(ctx: sql.Expr_concatContext): MType =
    new MString(visit(ctx.expr(0)).toString + visit(ctx.expr(1)).toString)

  override def visitExpr_arithmetic_p1(ctx: sql.Expr_arithmetic_p1Context): MType = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.T_DIV)
      left.Divide(right)
    else if (ctx.T_MUL)
      left.Multiply(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_arithmetic_p2(ctx: sql.Expr_arithmetic_p2Context): MType = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.T_ADD)
      left.Add(right)
    else if (ctx.T_SUB)
      left.Subtract(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_compare(ctx: sql.Expr_compareContext): MType = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.compare_operator.T_EQUAL || ctx.compare_operator.T_EQUAL2)
      left.Equal(right)
    else if (ctx.compare_operator.T_NOTEQUAL)
      left.NotEqual(right)
    else if (ctx.compare_operator.T_GREATER)
      left.MoreThen(right)
    else if (ctx.compare_operator.T_GREATEREQUAL)
      left.MoreEqualThen(right)
    else if (ctx.compare_operator.T_LESS)
      left.LessThen(right)
    else if (ctx.compare_operator.T_LESSEQUAL)
      left.LessEqualThen(right)
    else
      throw new UnsupportedOperationException("unknown compare operator")
  }

  override def visitExpr_logical(ctx: sql.Expr_logicalContext): MType = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.logical_operator.T_OR)
      left.Or(right)
    else if (ctx.logical_operator.T_AND)
      left.And(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_not(ctx: sql.Expr_notContext): MType = visit(ctx.expr).Not()

  override def visitExpr_recurse(ctx: sql.Expr_recurseContext): MType =
    if (ctx.expr)
      visit(ctx.expr)
    else if (ctx.other)
      executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
        case Success(value) => value
        case Failure(exception) =>
          if (context.errorSkip)
            MNull.get()
          else
            throw exception
      }
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_case(ctx: sql.Expr_caseContext): MType = {
    val switch =
      if (ctx.case_r.ex_switch)
        Some(visit(ctx.case_r.ex_switch))
      else
        None
    ctx.case_r.case_when_then.forEach(case_r => {
      val condition: Boolean =
        if (switch.nonEmpty)
          switch.get == visit(case_r.condition)
        else
          visit(case_r.condition)
      if (condition)
        return visit(case_r.ex_do)
    })
    if (ctx.case_r.ex_else)
      return visit(ctx.case_r.ex_else)
    MNull.get() // TODO default result if no condition matched (mb exception?)
  }

  override def visitExpr_index(ctx: sql.Expr_indexContext): MType = {
    val col = visit(ctx.collection)
    col match {
      case x: MCollection => x(visit(ctx.index))
      case _              => throw new NoSuchMethodException("only collections supports access by index")
    }
  }

  override def visitExpr_lambda(ctx: sql.Expr_lambdaContext): MType = {
    val pNames = ctx.lambda.ident.asScala.map(n => visit(n).toString).toList
    new MLambda(pNames, ctx.lambda.block, this.tokenStream)
  }

  override def visitExpr_async(ctx: sql.Expr_asyncContext): MType = {
    visit(ctx.async)
  }

  override def visitAsync(ctx: sql.AsyncContext): MType = {
    val fut: Future[Any] = Future[Any] {
      new MLambda(Nil, ctx.block, this.tokenStream)(context)
    }
    new MAsync(fut)
  }

  override def visitExpr_await(ctx: sql.Expr_awaitContext): MType = {
    val res =
      if (ctx.await.func)
        visit(ctx.await.func)
      else if (ctx.await.`var`)
        visit(ctx.await.`var`)
      else
        visit(ctx.await.async)
    if (res.isInstanceOf[MAsync])
      res.asInstanceOf[MAsync].await()
    else
      throw new IllegalCallerException("can await only async call")
  }

  override def visitExpr_func(ctx: sql.Expr_funcContext): MType = {
    visit(ctx.func)
  }

  override def visitFunc(ctx: sql.FuncContext): MType = {
    val funcName = visit(ctx.ident).toString
    // TODO: add the implicit cast
    val args: Seq[Object] =
      ctx.arg.asScala.flatMap(arg => {
        if (arg.ident == null)
          Seq(unpack(visit(arg.expr)).asInstanceOf[Object])
        else
          Nil
      }).toSeq
    val kwargs: Map[String, Object] =
      ctx.arg.asScala.flatMap(arg => {
        if (arg.ident != null)
          Seq(visit(arg.ident).toString -> unpack(visit(arg.expr)).asInstanceOf[Object])
        else
          Nil
      }).toMap
    if (ctx.T_ASYNC) {
      new MAsync(
        FunctionInvoker.invokeAsync(context.functions.toMap, funcName, List[Object](context), args.toList, kwargs)
      )
    } else {
      val res = FunctionInvoker.invoke(context.functions.toMap, funcName, List[Object](context), args.toList, kwargs)
      controlState = ControlContext.NONE
      pack(res)
    }
  }

  override def visitExprSpecFuncCast(ctx: sql.ExprSpecFuncCastContext): MType = {
    if (ctx.dtype.primitive_type)
      castPrimitive(visit(ctx.expr), ctx.dtype.primitive_type)
    else
      throw new UnsupportedOperationException("unknown type")
  }

  def castPrimitive(value: MType, to: sql.Primitive_typeContext): MType = {
    if (to.T_STRING)
      typeConversion.to_string(value)
    else if (to.T_INT)
      typeConversion.to_int(value)
    else if (to.T_DOUBLE)
      typeConversion.to_double(value)
    else if (to.T_BOOL)
      typeConversion.to_bool(value)
    else
      throw new ClassCastException("could not cast to this type")
  }
}
