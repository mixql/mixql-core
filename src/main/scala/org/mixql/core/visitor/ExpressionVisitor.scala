package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.parser.sql
import org.mixql.core.function.SqlLambda
import org.mixql.core.function.FunctionInvoker
import org.antlr.v4.runtime.misc.Interval

import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

trait ExpressionVisitor extends BaseVisitor:
  def executeOther(stmt: String, engine: sql.Choose_engineContext): Try[Type] =
    Try {
      if engine then
        // execute on custom engine
        // get engine name
        val engineName =
          if engine.expr then
            visit(engine.expr).toString
          else
            visit(engine.ident).toString
        if engine.engine_params then
          // execute with additional params
          val params = engine.engine_params.ident.asScala
            .map(visit(_).toString)
            .zip(engine.engine_params.expr.asScala.map(visit))
            .toMap
          context.execute(stmt, engineName, params)
        else
          // execute with current params
          context.execute(stmt, engineName)
      else
        // execute on current engine
        context.execute(stmt)
    }

  override def visitVar(ctx: sql.VarContext): Type =
    context.getVar(visit(ctx.ident).toString)

  override def visitExpr_concat(ctx: sql.Expr_concatContext): Type =
    string(visit(ctx.expr(0)).toString + visit(ctx.expr(1)).toString)

  override def visitExpr_arithmetic_p1(
    ctx: sql.Expr_arithmetic_p1Context
  ): Type =
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if ctx.T_DIV then
      left / right
    else if ctx.T_MUL then
      left * right
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_arithmetic_p2(
    ctx: sql.Expr_arithmetic_p2Context
  ): Type =
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if ctx.T_ADD then
      left + right
    else if ctx.T_SUB then
      left - right
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_compare(ctx: sql.Expr_compareContext): Type =
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if ctx.compare_operator.T_EQUAL || ctx.compare_operator.T_EQUAL2 then
      left == right
    else if ctx.compare_operator.T_NOTEQUAL then
      left != right
    else if ctx.compare_operator.T_GREATER then
      left > right
    else if ctx.compare_operator.T_GREATEREQUAL then
      left >= right
    else if ctx.compare_operator.T_LESS then
      left < right
    else if ctx.compare_operator.T_LESSEQUAL then
      left <= right
    else
      throw new UnsupportedOperationException("unknown compare operator")

  override def visitExpr_logical(ctx: sql.Expr_logicalContext): Type =
    val left = visit(ctx.expr(0))
    val right = visit(ctx.expr(1))
    if ctx.logical_operator.T_OR then
      left || right
    else if ctx.logical_operator.T_AND then
      left && right
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_not(ctx: sql.Expr_notContext): Type =
    visit(ctx.expr).!()

  override def visitExpr_recurse(ctx: sql.Expr_recurseContext): Type =
    if ctx.expr then
      visit(ctx.expr)
    else if ctx.other then
      executeOther(visit(ctx.other).toString, ctx.choose_engine) match
        case Success(value) => value
        case Failure(exception) =>
          if context.grenkiErrorSkip then Null else throw exception
    else
      throw new UnsupportedOperationException("unknown operator")

  override def visitExpr_case(ctx: sql.Expr_caseContext): Type =
    val switch =
      if ctx.case_r.ex_switch then
        Some(visit(ctx.case_r.ex_switch))
      else
        None
    ctx.case_r.case_when_then
      .forEach(case_r => {
        val condition: Boolean =
          if switch.nonEmpty then
            switch.get == visit(case_r.condition)
          else
            visit(case_r.condition)
        if condition then return visit(case_r.ex_do)
      })
    if ctx.case_r.ex_else then return visit(ctx.case_r.ex_else)
    Null // TODO default result if no condition matched (mb exception?)

  override def visitExpr_index(ctx: sql.Expr_indexContext): Type =
    val col = visit(ctx.collection)
    col match
      case x: collection => x(visit(ctx.index))
      case _ =>
        throw new NoSuchMethodException(
          "only collections supports access by index"
        )

  override def visitExpr_lambda(ctx: sql.Expr_lambdaContext): Type =
    val pNames = ctx.lambda.ident.asScala.map(n => visit(n).toString).toList
    new SqlLambda(pNames, ctx.lambda.block, this)

  override def visitExpr_func(ctx: sql.Expr_funcContext): Type =
    val funcName = visit(ctx.func.ident(0)).toString
    // TODO: add the implicit cast
    val params: Seq[Any] = ctx.func.expr.asScala
      .map(visit)
      .map(unpack)
      .toSeq
    pack(FunctionInvoker.invoke(context.functions.toMap, funcName, params))

  override def visitExprSpecFuncCast(ctx: sql.ExprSpecFuncCastContext): Type =
    if ctx.dtype.primitive_type then
      castPrimitive(visit(ctx.expr), ctx.dtype.primitive_type)
    else if ctx.dtype.array_type then
      throw new UnsupportedOperationException("array types not supported now")
    else if ctx.dtype.map_type then
      throw new UnsupportedOperationException("map types not supported now")
    else if ctx.dtype.struct_type then
      throw new UnsupportedOperationException("struct types not supported now")
    else if ctx.dtype.user_defined_type then
      throw new UnsupportedOperationException(
        "user defined types not supported now"
      )
    else
      throw new UnsupportedOperationException("unknown type")

  def castPrimitive(value: Type, to: sql.Primitive_typeContext): Type =
    if to.T_STRING then
      typeConversion.to_string(value)
    else if to.T_INT then
      typeConversion.to_int(value)
    else if to.T_DOUBLE then
      typeConversion.to_double(value)
    else if to.T_BOOL then
      typeConversion.to_bool(value)
    else
      throw new ClassCastException("could not cast to this type")
