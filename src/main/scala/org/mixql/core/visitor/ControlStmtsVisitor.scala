package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.parser.sql
import scala.language.implicitConversions

trait ControlStmtsVisitor extends BaseVisitor:
  override def visitTry_catch_stmt(ctx: sql.Try_catch_stmtContext): Type =
    try
      visit(ctx.try_bock)
    catch
      case e: Throwable =>
        val old_exc =
          if ctx.exc then
            context.getVar(visit(ctx.exc).toString)
          else
            null
        val old_message =
          if ctx.exc then
            context.getVar(visit(ctx.exc).toString + ".message")
          else
            null
        if old_exc != null then
          context.setVar(
            visit(ctx.exc).toString,
            string(e.getClass.getSimpleName)
          )
        if old_message != null then
          context.setVar(
            visit(ctx.exc).toString + ".message",
            string(e.getMessage)
          )
        visit(ctx.catch_block)
        if old_exc != null then
          context.setVar(visit(ctx.exc).toString, old_exc)
        if old_message != null then
          context.setVar(visit(ctx.exc).toString + ".message", old_message)
    Null

  override def visitIf_stmt(ctx: sql.If_stmtContext): Type =
    val condition: Boolean = visit(ctx.expr)
    if condition then
      visit(ctx.block)
    else
      ctx
        .elseif_block()
        .forEach(elif => {
          val elsecondition: Boolean = visit(elif.expr)
          if elsecondition then {
            return visit(elif.block)
          }
        })
      if ctx.else_block then
        visit(ctx.else_block.block)
      else
        Null

  // TODO maybe better realisation using for?
  override def visitFor_range_stmt(ctx: sql.For_range_stmtContext): Type =
    // super.visitFor_range_stmt(ctx)
    val i_name = visit(ctx.ident).toString
    val old = context.getVar(i_name)
    var i = if !ctx.T_REVERSE then visit(ctx.from) else visit(ctx.to)
    val to = if !ctx.T_REVERSE then visit(ctx.to) else visit(ctx.from)
    val step =
      (if ctx.T_REVERSE then int(-1) else int(1)) * (if ctx.step then visit(ctx.step)
      else int(1))
    context.setVar(i_name, i)

    while ((!ctx.T_REVERSE && i < to) || (ctx.T_REVERSE && i > to))
      visit(ctx.block)
      i = i + step
      context.setVar(i_name, i)

    if ((!ctx.T_REVERSE && i >= to) || (ctx.T_REVERSE && i <= to))
      context.setVar(i_name, to)
      visit(ctx.block)


    context.setVar(i_name, old)
    Null

  override def visitFor_cursor_stmt(ctx: sql.For_cursor_stmtContext): Type =
    val cursor = visit(ctx.expr)
    val cursorName = visit(ctx.ident).toString
    val old = context.getVar(cursorName)
    cursor match
      case array(arr) =>
        arr.foreach(el => {
          context.setVar(cursorName, el)
          visit(ctx.block)
        })
      case other =>
        throw new IllegalArgumentException("cursor must be collection")
    context.setVar(cursorName, old)
    Null

  override def visitWhile_stmt(ctx: sql.While_stmtContext): Type =
    var condition: Boolean = visit(ctx.expr)
    while condition do
      visit(ctx.block)
      condition = visit(ctx.expr)
    Null
