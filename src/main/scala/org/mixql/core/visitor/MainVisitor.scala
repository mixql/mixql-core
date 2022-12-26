package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval
import org.mixql.core.context.Context
import org.mixql.core.context.gtype._
import org.mixql.core.function.SqlLambda
import org.mixql.core.parser.sql

import scala.util.{Failure, Success}
import scala.collection.JavaConverters._

class MainVisitor(ctx: Context, tokens: TokenStream)
    extends ExpressionVisitor
    with LiteralVisitor
    with ControlStmtsVisitor {
  val context = ctx
  val tokenStream = tokens

  override def visitProgram(ctx: sql.ProgramContext): Type =
    visit(ctx.block)

  override def visitBlock(ctx: sql.BlockContext): Type = {
    var res: Type = Null
    ctx.statment.asScala.foreach(stmt => {
      res = visit(stmt)
      if (res.ret)
        return res
    })
    res
  }

  override def visitEmpty_stmt(x: sql.Empty_stmtContext): Type = Null

  override def visitReturn_stmt(ctx: sql.Return_stmtContext): Type = {
    val res = visit(ctx.expr)
    res.ret = true
    res
  }

  override def visitExpr_stmt(ctx: sql.Expr_stmtContext): Type = {
    visit(ctx.expr)
  }

  override def visitChange_engine_stmt(
    ctx: sql.Change_engine_stmtContext
  ): Type = {
    if (ctx.choose_engine.expr)
      context.setCurrentEngine(visit(ctx.choose_engine.expr).toString)
    else
      context.setCurrentEngine(visit(ctx.choose_engine.ident).toString)
    if (ctx.choose_engine.engine_params)
      ctx.choose_engine.engine_params.ident.asScala
        .map(visit)
        .zip(ctx.choose_engine.engine_params.expr.asScala.map(visit))
        .foreach(p => context.currentEngine.setParam(p._1.toString, p._2))
    Null
  }

  override def visitAssigment_default(
    ctx: sql.Assigment_defaultContext
  ): Type = {
    val value = visit(ctx.expr)
    value match {
      case v: SqlLambda => context.addFunction(visit(ctx.ident).toString, v)
      case other => context.setVar(visit(ctx.ident).toString, visit(ctx.expr))
    }
    Null
  }

  override def visitAssigment_by_index(
    ctx: sql.Assigment_by_indexContext
  ): Type = {
    context.getVar(visit(ctx.ident).toString) match {
      case x: collection => x.update(visit(ctx.index), visit(ctx.value))
      case _ =>
        throw new NoSuchMethodException(
          "only collections supports access by index"
        )
    }
    Null
  }

  override def visitAssigment_multiple(ctx: sql.Assigment_multipleContext): Type = {
    if (ctx.expr.size > 1) {
      if (ctx.ident.size > ctx.expr.size)
        throw new IndexOutOfBoundsException("not enought argument for multiple assigment")
      ctx.ident.asScala.zip(ctx.expr.asScala).foreach(variable =>
        context.setVar(visit(variable._1).toString, visit(variable._2))
      )
    } else {
      val res = visit(ctx.expr(0)) match {
        case arr: array => 
          if (ctx.ident.size > arr.size.value)
            throw new IndexOutOfBoundsException("not enought argument for multiple assigment")
          ctx.ident.asScala.zip(arr.arr).foreach(variable =>
            context.setVar(visit(variable._1).toString, variable._2)
          )
        case _ => throw new IllegalArgumentException("cannot unpack non array expression") 
      }
    }
    Null  
  }

  override def visitPrint_stmt(ctx: sql.Print_stmtContext): Type = {
    println("[USER PRINT]: " + visit(ctx.expr).toString)
    Null
  }

  override def visitOther_stmt(ctx: sql.Other_stmtContext): Type = {
    executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
      case Success(value) => value
      case Failure(exception) =>
        if (context.grenkiErrorSkip) Null else throw exception
    }
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

  override def visitInterpolation_expr(
    ctx: sql.Interpolation_exprContext
  ): Type = {
    visit(ctx.expr)
  }
}
