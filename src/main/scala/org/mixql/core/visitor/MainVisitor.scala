package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval
import org.mixql.core.context.{Context, EngineContext, ControlContext}
import org.mixql.core.context.mtype._
import org.mixql.core.generated.sql
import org.mixql.core.generated.sql.{Close_cursor_stmtContext, Open_cursor_stmtContext}
import org.mixql.core.logger.{logDebug, logInfo, logWarn}

import scala.collection.mutable.{Map => MutMap}
import scala.util.{Failure, Success}
import scala.collection.JavaConverters._
import org.mixql.core.exception.MException
import scala.collection.mutable.Buffer

/** it is not thread safe. if you need new multithread run new visitor for each
  *
  * @param ctx
  * @param tokens
  */
class MainVisitor(ctx: Context, tokens: TokenStream)
    extends ExpressionVisitor
    with LiteralVisitor
    with ControlStmtsVisitor {

  val context = ctx
  val tokenStream = tokens

  var controlState = ControlContext.NONE

  override def visitProgram(ctx: sql.ProgramContext): MType = visit(ctx.block)

  override def visitBlock(ctx: sql.BlockContext): MType = {
    var res: MType = MNull.get()
    ctx.statment.asScala.foreach(stmt => {
      res = visit(stmt)
      if (controlState == ControlContext.CONTINUE) {
        controlState = ControlContext.NONE
        return res
      }
      if (controlState != ControlContext.NONE)
        return res
    })
    res
  }

  override def visitFor_cursor_stmt(ctx: sql.For_cursor_stmtContext): MType = {

    if (ctx.T_CURSOR) {
      val anonymousCursor = new MCursor(this.ctx, tokens, ctx.expr)
      execForInMcursor(anonymousCursor, ctx)
      anonymousCursor.close()
    } else {
      val exprRes = visit(ctx.expr)
      if (exprRes.isInstanceOf[MCursor]) {
        val cursor = exprRes.asInstanceOf[MCursor]
        execForInMcursor(cursor, ctx)
      } else {
        exprRes match {
          case collection: MCollection => execBlockInFor(collection, ctx)
          case _ =>
            logWarn("\"visitFor_cursor_stmt\": cursor must be collection. Ignore executing of for block")
            MNull.get()
          //            throw new IllegalArgumentException("cursor must be collection")
        }
      }
    }
  }

  override def visitEmpty_stmt(x: sql.Empty_stmtContext): MType = MNone.get()

  override def visitReturn_stmt(ctx: sql.Return_stmtContext): MType = {
    val res = visit(ctx.expr)
    controlState = ControlContext.RETURN
    res
  }

  override def visitBreak_stmt(ctx: sql.Break_stmtContext): MType = {
    controlState = ControlContext.BREAK
    MNone.get()
  }

  override def visitContinue_stmt(ctx: sql.Continue_stmtContext): MType = {
    controlState = ControlContext.CONTINUE
    MNone.get()
  }

  override def visitRaise_stmt(ctx: sql.Raise_stmtContext): MType = {
    if (ctx.exc_type) {
      val exc_type = visit(ctx.exc_type).toString
      val exc_message =
        if (ctx.exc_message)
          visit(ctx.exc_message).toString
        else
          ""
      throw new MException(exc_type, exc_message)
    }
    throw new MException("UserError", "")
  }

  override def visitExpr_stmt(ctx: sql.Expr_stmtContext): MType = {
    visit(ctx.expr)
  }

  override def visitChange_engine_stmt(ctx: sql.Change_engine_stmtContext): MType = {
    val engine_name =
      if (ctx.choose_engine.expr)
        visit(ctx.choose_engine.expr).toString
      else
        visit(ctx.choose_engine.ident).toString
    if (ctx.choose_engine.engine_params) {
      val params = ctx.choose_engine.engine_params.ident.asScala.map(x => visit(x).toString)
        .zip(ctx.choose_engine.engine_params.expr.asScala.map(visit))
      context.setCurrentEngine(engine_name, MutMap() ++ params.toMap)
    } else {
      context.setCurrentEngine(engine_name)
    }
    MNone.get()
  }

  override def visitOpen_cursor_stmt(ctx: Open_cursor_stmtContext): MBool = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor: MCursorBase => cursor.open()
      case _ =>
        throw new Exception(
          "You can only open cursor, not other type: " +
            cursor.getClass.getName
        )
    }
  }

  override def visitClose_cursor_stmt(ctx: Close_cursor_stmtContext): MBool = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor: MCursorBase =>
        val res = cursor.close()
        if (res.getValue) { context.setVar(cursor_name, MNull.get()) }
        res
      case _ =>
        throw new Exception(
          "You can only close cursor, not other type: " +
            cursor.getClass.getName
        )
    }
  }

  override def visitExpr_fetch_cursor(ctx: sql.Expr_fetch_cursorContext): MType = {
    val cursor_name = visit(ctx.ident).toString
    val cursor = context.getVar(cursor_name)
    cursor match {
      case cursor: MCursorBase =>
        val res = cursor.fetch()
        logDebug("visitExpr_fetch_cursor: returning from fetch " + res)
        res
      case _ =>
        throw new Exception(
          "You can only fetch from cursor, not other type: " +
            cursor.getClass.getName
        )
    }
  }

  override def visitAssigment_default(ctx: sql.Assigment_defaultContext): MType = {
    val value: MType =
      if (ctx.T_CURSOR() && ctx.T_IS()) {
        val value = new MCursor(context, tokenStream, ctx.expr())
        logDebug("assign cursor")
        value
      } else {
        val value = visit(ctx.expr)
        logDebug("assign value: " + value)
        value
      }
    if (ctx.T_GLOBAL)
      context.setGlobalVar(visit(ctx.ident).toString, value)
    else
      context.setVar(visit(ctx.ident).toString, value)
    MNone.get()
  }

  override def visitAssigment_by_index(ctx: sql.Assigment_by_indexContext): MType = {
    val coll =
      if (ctx.T_GLOBAL)
        context.getGlobalVar(visit(ctx.ident).toString)
      else
        context.getVar(visit(ctx.ident).toString)
    coll match {
      case x: MCollection => x.update(visit(ctx.index), visit(ctx.value))
      case _              => throw new NoSuchMethodException("only collections supports access by index")
    }
    MNone.get()
  }

  override def visitAssigment_multiple(ctx: sql.Assigment_multipleContext): MType = {
    val varsToSet: Buffer[(sql.IdentContext, MType)] =
      if (ctx.expr.size > 1) {
        if (ctx.ident.size > ctx.expr.size)
          throw new IndexOutOfBoundsException("not enought argument for multiple assigment")
        ctx.ident.asScala.zip(ctx.expr.asScala).map(x => x._1 -> visit(x._2))
      } else {
        visit(ctx.expr(0)) match {
          case arr: MArray =>
            if (ctx.ident.size > arr.size.getValue)
              throw new IndexOutOfBoundsException("not enought argument for multiple assigment")
            ctx.ident.asScala.zip(arr.getArr)
          case _ => throw new IllegalArgumentException("cannot unpack non array expression")
        }
      }
    if (ctx.T_GLOBAL)
      varsToSet.foreach(variable => context.setGlobalVar(visit(variable._1).toString, variable._2))
    else
      varsToSet.foreach(variable => context.setVar(visit(variable._1).toString, variable._2))
    MNone.get()
  }

  override def visitPrint_stmt(ctx: sql.Print_stmtContext): MType = {
    println("[USER PRINT]: " + visit(ctx.expr).toString)
    MNone.get()
  }

  override def visitOther_stmt(ctx: sql.Other_stmtContext): MType = {
    executeOther(visit(ctx.other).toString, ctx.choose_engine) match {
      case Success(value) => value
      case Failure(exception) =>
        if (context.errorSkip)
          MNull.get()
        else
          throw exception
    }
  }

  override def visitOther(ctx: sql.OtherContext): MType = {
    var res = ""
    var from = ctx.start.getTokenIndex
    var to = from
    if (ctx.children != null) {
      ctx.children.forEach(child => {
        to = child.getSourceInterval.a - 1
        val ch =
          visit(child) match {
            case s: MString => s.quoted
            case other      => other.toString
          }
        res += tokenStream.getText(new Interval(from, to)) + ch
        from = child.getSourceInterval.b + 1
      })
    }
    new MString(res)
  }

  override def visitInterpolation_expr(ctx: sql.Interpolation_exprContext): MType = {
    new MString(visit(ctx.expr).toString)
  }
}
