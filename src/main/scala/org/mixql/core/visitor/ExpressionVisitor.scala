package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.function.SqlLambda
import org.mixql.core.function.FunctionInvoker
import org.antlr.v4.runtime.misc.Interval
import org.mixql.core.generated.sql

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import org.mixql.core.context.ControlContext

trait ExpressionVisitor extends BaseVisitor {
  def executeOther(stmt: String, engine: sql.Choose_engineContext): Try[Type] =
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

  override def visitVar(ctx: sql.VarContext): Type = {
    val res = context.getVar(visit(ctx.ident).toString)
    if (res.isInstanceOf[string])
      new string(res.toString)
    else
      res
  }

  override def visitExpr_concat(ctx: sql.Expr_concatContext): Type =
    new string(visit(ctx.expr(0)).toString + visit(ctx.expr(1)).toString)

  override def visitExpr_arithmetic_p1(ctx: sql.Expr_arithmetic_p1Context): Type = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.T_DIV)
      left.Divide(right)
    else if (ctx.T_MUL)
      left.Multiply(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_arithmetic_p2(ctx: sql.Expr_arithmetic_p2Context): Type = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.T_ADD)
      left.Add(right)
    else if (ctx.T_SUB)
      left.Subtract(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_compare(ctx: sql.Expr_compareContext): Type = {
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

  override def visitExpr_logical(ctx: sql.Expr_logicalContext): Type = {
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if (ctx.logical_operator.T_OR)
      left.Or(right)
    else if (ctx.logical_operator.T_AND)
      left.And(right)
    else
      throw new UnsupportedOperationException("unknown operator")
  }

  override def visitExpr_not(ctx: sql.Expr_notContext): Type = visit(ctx.expr).Not()

  override def visitExpr_recurse(ctx: sql.Expr_recurseContext): Type =
    if (ctx.expr)
      visit(ctx.expr)
    else if (ctx.other)
      executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
        case Success(value) => value
        case Failure(exception) =>
          if (context.errorSkip)
            new Null()
          else
            throw exception
      }
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_case(ctx: sql.Expr_caseContext): Type = {
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
    new Null() // TODO default result if no condition matched (mb exception?)
  }

  override def visitExpr_index(ctx: sql.Expr_indexContext): Type = {
    val col = visit(ctx.collection)
    col match {
      case x: collection => x(visit(ctx.index))
      case _             => throw new NoSuchMethodException("only collections supports access by index")
    }
  }

  override def visitExpr_lambda(ctx: sql.Expr_lambdaContext): Type = {
    val pNames = ctx.lambda.ident.asScala.map(n => visit(n).toString).toList
    new SqlLambda(pNames, ctx.lambda.block, this)
  }

  override def visitExpr_func(ctx: sql.Expr_funcContext): Type = {
    val funcName = visit(ctx.func.ident).toString
    // TODO: add the implicit cast
    val args: Seq[Object] =
      ctx.func.arg.asScala.flatMap(arg => {
        if (arg.ident == null)
          Seq(unpack(visit(arg.expr)).asInstanceOf[Object])
        else
          Nil
      }).toSeq
    val kwargs: Map[String, Object] =
      ctx.func.arg.asScala.flatMap(arg => {
        if (arg.ident != null)
          Seq(visit(arg.ident).toString -> unpack(visit(arg.expr)).asInstanceOf[Object])
        else
          Nil
      }).toMap
    val res = FunctionInvoker.invoke(context.functions.toMap, funcName, context, args.toList, kwargs)
    controlState = ControlContext.NONE
    pack(res)
  }

  override def visitExprSpecFuncCast(ctx: sql.ExprSpecFuncCastContext): Type = {
    if (ctx.dtype.primitive_type)
      castPrimitive(visit(ctx.expr), ctx.dtype.primitive_type)
//    else if (ctx.dtype.array_type)
//      throw new UnsupportedOperationException("array types not supported now")
//    else if (ctx.dtype.map_type)
//      throw new UnsupportedOperationException("map types not supported now")
//    else if (ctx.dtype.struct_type)
//      throw new UnsupportedOperationException("struct types not supported now")
//    else if (ctx.dtype.user_defined_type)
//      throw new UnsupportedOperationException(
//        "user defined types not supported now"
//      )
    else
      throw new UnsupportedOperationException("unknown type")
  }

  def castPrimitive(value: Type, to: sql.Primitive_typeContext): Type = {
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
