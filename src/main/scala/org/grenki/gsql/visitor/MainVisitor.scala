package org.grenki.gsql.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.function.FunctionInvoker
import org.grenki.gsql.sql

import scala.util.{Success, Failure}
import scala.jdk.CollectionConverters._

class MainVisitor(ctx: Context, tokens: TokenStream)
    extends ExpressionVisitor
    with LiteralVisitor
    with ControlStmtsVisitor {

  val context = ctx
  val tokenStream = tokens

  override def visitOther_stmt(ctx: sql.Other_stmtContext): Type = {
    executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
      case Success(value) => value
      case Failure(exception) =>
        if (context.grenkiErrorSkip) Null else throw exception
    }
  }

  override def visitChoose_engine(ctx: sql.Choose_engineContext): Type = {
    if (ctx.expr)
      context.setCurrentEngine(visit(ctx.expr).toString)
    else
      context.setCurrentEngine(visit(ctx.ident).toString)
    if (ctx.engine_params)
      ctx.engine_params.ident.asScala
        .map(visit)
        .zip(ctx.engine_params.expr.asScala.map(visit))
        .foreach(p => context.currentEngine.setParam(p._1.toString, p._2))
    Null
  }
  override def visitAssigment_stmt(ctx: sql.Assigment_stmtContext): Type = {
    context.setVar(visit(ctx.ident).toString, visit(ctx.expr))
    Null
  }

  override def visitOther(ctx: sql.OtherContext): Type = {
    var res = ""
    var from = ctx.start.getTokenIndex
    var to = from
    ctx.children.forEach(child => {
      to = child.getSourceInterval.a - 1
      val ch = visit(child) match {
        case s: string => s.quoted
        case other     => other.toString
      }
      res += tokenStream.getText(new Interval(from, to)) + ch
      from = child.getSourceInterval.b + 1
    })
    string(res)
  }

  override def visitVar(ctx: sql.VarContext): Type = {
    context.getVar(visit(ctx.ident).toString)
  }

  override def visitInterpolation_exp(
    ctx: sql.Interpolation_expContext
  ): Type = {
    visit(ctx.expr)
  }

  override def visitPrint_stmt(ctx: sql.Print_stmtContext): Type = {
    println("[USER PRINT]: " + visit(ctx.expr).toString)
    Null
  }

  override def visitExpr_func(ctx: sql.Expr_funcContext): Type = {
    val func = ctx.func()
    val ident = func.ident(0)
    val funcName = tokenStream.getText(
      new Interval(ident.start.getTokenIndex, ident.stop.getTokenIndex)
    )
    // todo: add the implicit cast
    val params: Seq[Any] = func.expr.asScala
      .map(visit)
      .map {
        case Null         => null
        case string(v, q) => v
        case int(v)       => v
        case double(v)    => v
      }
      .toSeq
    FunctionInvoker.invoke(context.functions.toMap, funcName, params) match {
      case p: String => string(p)
      case p: Int    => int(p)
    }
  }
}
