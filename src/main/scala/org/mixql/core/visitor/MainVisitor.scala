package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval
import org.mixql.core.context.{Context, ContextVars}
import org.mixql.core.context.gtype._
import org.mixql.core.function.SqlLambda
import org.mixql.core.generated.sql
import org.mixql.core.generated.sql.{Close_cursor_stmtContext, Open_cursor_stmtContext}
import org.mixql.core.logger.{logDebug, logInfo, logWarn}

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
    var res: Type = new Null()
    ctx.statment.asScala.foreach(stmt => {
      res = visit(stmt)
      if (res.control != Type.Control.NONE)
        return res
    })
    res
  }

  override def visitFor_cursor_stmt(ctx: sql.For_cursor_stmtContext): Type = {

    if (ctx.T_CURSOR != null) {
      val anonymousCursor = new gcursor(this.ctx, tokens, ctx.expr)
      execForInGcursor(anonymousCursor, ctx)
      anonymousCursor.close()
    } else {
      val exprRes = visit(ctx.expr)
      if (exprRes.isInstanceOf[gcursor]) {
        val cursor = exprRes.asInstanceOf[gcursor]
        execForInGcursor(cursor, ctx)
      } else {
        exprRes match {
          case collection1: collection => execBlockInFor(collection1, ctx)
          case _ =>
            logWarn("\"visitFor_cursor_stmt\": cursor must be collection. Ignore executing of for block")
            new Null()
          //            throw new IllegalArgumentException("cursor must be collection")
        }
      }
    }
  }

  override def visitEmpty_stmt(x: sql.Empty_stmtContext): Type = new Null()

  override def visitReturn_stmt(ctx: sql.Return_stmtContext): Type = {
    val res = visit(ctx.expr)
    res.control = Type.Control.RETURN
    res
  }

  override def visitBreak_stmt(ctx: sql.Break_stmtContext): Type = {
    val res = new Null
    res.control = Type.Control.BREAK
    res
  }

  override def visitContinue_stmt(ctx: sql.Continue_stmtContext): Type = {
    val res = new Null
    res.control = Type.Control.CONTINUE
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
        .foreach(p => {
          context.setVar(p._1.toString, p._2)
          context.currentEngine._paramChanged(p._1.toString, new ContextVars(context))
        })
    new Null()
  }

  override def visitAssigment_default(
                                       ctx: sql.Assigment_defaultContext
                                     ): Type = {
    if (ctx.T_CURSOR() != null && ctx.T_IS() != null) {
      val cursor = new gcursor(context, tokenStream, ctx.expr())
      context.setVar(visit(ctx.ident).toString, cursor)
    } else {
      val value = visit(ctx.expr)
      logDebug("visitAssigment_default: value: " + value)
      value match {
        case v: SqlLambda =>
          val lambdaFuncName = visit(ctx.ident).toString;
          logDebug("Adding lambda function " + lambdaFuncName + " to context")
          context.addFunction(lambdaFuncName, v)

        case _ => context.setVar(visit(ctx.ident).toString, value)
      }
    }
    new Null()
  }

  override def visitOpen_cursor_stmt(ctx: Open_cursor_stmtContext): bool = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor1: cursor =>
        cursor1.open()
      case _ =>
        throw new Exception("You can only open cursor, not other type: " +
          cursor.getClass.getName
        )
    }
  }

  override def visitClose_cursor_stmt(ctx: Close_cursor_stmtContext): bool = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor1: cursor =>
        val res = cursor1.close()
        if (res.getValue) {
          context.setVar(cursor_name, new Null())
        }
        res
      case _ =>
        throw new Exception("You can only close cursor, not other type: " +
          cursor.getClass.getName
        )
    }
  }

  override def visitExpr_fetch_cursor(ctx: sql.Expr_fetch_cursorContext): Type = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor1: cursor =>
        val res = cursor1.fetch()
        logDebug("visitExpr_fetch_cursor: returning from fetch " + res)
        res
      case _ =>
        throw new Exception("You can only fetch from cursor, not other type: " +
          cursor.getClass.getName
        )
    }
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
    new Null()
  }

  override def visitAssigment_multiple(
                                        ctx: sql.Assigment_multipleContext
                                      ): Type = {
    if (ctx.expr.size > 1) {
      if (ctx.ident.size > ctx.expr.size)
        throw new IndexOutOfBoundsException(
          "not enought argument for multiple assigment"
        )
      ctx.ident.asScala
        .zip(ctx.expr.asScala)
        .foreach(variable =>
          context.setVar(visit(variable._1).toString, visit(variable._2))
        )
    } else {
      val res = visit(ctx.expr(0)) match {
        case arr: array =>
          if (ctx.ident.size > arr.size.getValue)
            throw new IndexOutOfBoundsException(
              "not enought argument for multiple assigment"
            )
          ctx.ident.asScala
            .zip(arr.getArr)
            .foreach(variable =>
              context.setVar(visit(variable._1).toString, variable._2)
            )
        case _ =>
          throw new IllegalArgumentException(
            "cannot unpack non array expression"
          )
      }
    }
    new Null()
  }

  override def visitPrint_stmt(ctx: sql.Print_stmtContext): Type = {
    println("[USER PRINT]: " + visit(ctx.expr).toString)
    new Null()
  }

  override def visitOther_stmt(ctx: sql.Other_stmtContext): Type = {
    executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
      case Success(value) => value
      case Failure(exception) =>
        if (context.errorSkip) new Null() else throw exception
    }
  }

  override def visitOther(ctx: sql.OtherContext): Type = {
    var res = ""
    var from = ctx.start.getTokenIndex
    var to = from
    if (ctx.children != null) {
      ctx.children.forEach(child => {
        to = child.getSourceInterval.a - 1
        val ch = visit(child) match {
          case s: string => s.quoted
          case other => other.toString
        }
        res += tokenStream.getText(new Interval(from, to)) + ch
        from = child.getSourceInterval.b + 1
      })
    }
    new string(res)
  }

  override def visitInterpolation_expr(
                                        ctx: sql.Interpolation_exprContext
                                      ): Type = {
    new string(visit(ctx.expr).toString)
  }
}
