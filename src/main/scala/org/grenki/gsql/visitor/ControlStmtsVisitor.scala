package org.grenki.gsql.visitor

import org.grenki.gsql.context.gtype.{Type, int, Null}
import org.grenki.gsql.sql
import scala.language.implicitConversions
import org.grenki.gsql.context.gtype.string

trait ControlStmtsVisitor extends BaseVisitor {

  override def visitTry_catch_stmt(ctx: sql.Try_catch_stmtContext): Type = {
    try {
      visit(ctx.try_bock)
    } catch {
      case e: Throwable =>
        val old_exc =
          if (ctx.exc)
            context.getVar(visit(ctx.exc).toString)
          else
            null
        val old_message =
          if (ctx.exc)
            context.getVar(visit(ctx.exc).toString + ".message")
          else
            null
        if (old_exc != null) {
          context.setVar(
            visit(ctx.exc).toString,
            string(e.getClass.getSimpleName)
          )
        }
        if (old_message != null) {
          context.setVar(
            visit(ctx.exc).toString + ".message",
            string(e.getMessage)
          )
        }
        visit(ctx.catch_block)
        if (old_exc != null) {
          context.setVar(visit(ctx.exc).toString, old_exc)
        }
        if (old_message != null) {
          context.setVar(visit(ctx.exc).toString + ".message", old_message)
        }
    }
    Null
  }

  override def visitIf_stmt(ctx: sql.If_stmtContext): Type = {
    val condition: Boolean = visit(ctx.expr)
    if (condition) {
      visit(ctx.block)
      return Null
    } else {
      ctx
        .elseif_block()
        .forEach(elif => {
          val elsecondition: Boolean = visit(elif.expr)
          if (elsecondition) {
            visit(elif.block)
            return Null
          }
        })
      if (ctx.else_block) {
        visit(ctx.else_block.block)
        return Null
      }
    }
    Null
  }

  // TODO maybe better realisation using for?
  override def visitFor_range_stmt(ctx: sql.For_range_stmtContext): Type = {
    // super.visitFor_range_stmt(ctx)
    val i_name = visit(ctx.ident).toString
    val old = context.getVar(i_name)
    var i = if (!ctx.T_REVERSE) visit(ctx.from) else visit(ctx.to)
    val to = if (!ctx.T_REVERSE) visit(ctx.to) else visit(ctx.from)
    val step =
      (if (ctx.T_REVERSE) int(-1) else int(1)) * (if (ctx.step) visit(ctx.step)
                                                  else int(1))
    context.setVar(i_name, i)
    while (
      (!ctx.T_REVERSE && i < to) ||
      (ctx.T_REVERSE && i > to)
    ) {
      visit(ctx.block)
      i = i + step
      context.setVar(i_name, i)
    }
    if (
      (!ctx.T_REVERSE && i >= to) ||
      (ctx.T_REVERSE && i <= to)
    ) {
      context.setVar(i_name, to)
      visit(ctx.block)
    }
    context.setVar(i_name, old)
    Null
  }

  override def visitWhile_stmt(ctx: sql.While_stmtContext): Type = {
    var condition: Boolean = visit(ctx.expr)
    while (condition) {
      visit(ctx.block)
      condition = visit(ctx.expr)
    }
    Null
  }
}
