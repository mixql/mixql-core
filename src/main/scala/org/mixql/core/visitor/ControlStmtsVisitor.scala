package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.generated.sql

import scala.language.implicitConversions
import scala.collection.JavaConverters._

trait ControlStmtsVisitor extends BaseVisitor {
  override def visitTry_catch_stmt(ctx: sql.Try_catch_stmtContext): Type = {
    try {
      visit(ctx.try_bock)
    } catch {
      case e: Throwable =>
        val old_exc =
          if (ctx.exc)
            Some(context.getVar(visit(ctx.exc).toString))
          else
            None
        val old_message =
          if (ctx.exc)
            Some(context.getVar(visit(ctx.exc).toString + ".message"))
          else
            None
        if (old_exc.nonEmpty) {
          context.setVar(
            visit(ctx.exc).toString,
            string(e.getClass.getSimpleName)
          )
        }
        if (old_message.nonEmpty) {
          context.setVar(
            visit(ctx.exc).toString + ".message",
            string(e.getMessage)
          )
        }
        visit(ctx.catch_block)
        if (old_exc.nonEmpty) {
          context.setVar(visit(ctx.exc).toString, old_exc.get)
        }
        if (old_message.nonEmpty) {
          context.setVar(visit(ctx.exc).toString + ".message", old_message.get)
        }
    }
    Null
  }

  override def visitIf_stmt(ctx: sql.If_stmtContext): Type = {
    val condition: Boolean = visit(ctx.expr)
    if (condition) {
      visit(ctx.block)
    } else {
      ctx
        .elseif_block()
        .forEach(elif => {
          val elsecondition: Boolean = visit(elif.expr)
          if (elsecondition) {
            return visit(elif.block)
          }
        })
      if (ctx.else_block)
        visit(ctx.else_block.block)
      else
        Null
    }
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

  override def visitFor_cursor_stmt(ctx: sql.For_cursor_stmtContext): Type = {
    val cursor = visit(ctx.expr)
    cursor match {
      case array(arr) =>
        ctx.ident.size match {
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val old = context.getVar(cursorName)
            arr.foreach(el => {
              context.setVar(cursorName, el)
              visit(ctx.block)
            })
            context.setVar(cursorName, old)
          case other =>
            val cursors = ctx.ident.asScala.map(visit(_).toString).toList
            val old = cursors.map(context.getVar(_))
            arr.foreach(el => {
              if (el.isInstanceOf[array]) {
                val a = el.asInstanceOf[array]
                if (a.arr.size < cursors.size)
                  throw new IllegalStateException(
                    "not enough arguments to unpack"
                  )
                cursors
                  .zip(a.arr)
                  .foreach(kv => {
                    context.setVar(kv._1, kv._2)
                  })
                visit(ctx.block)
              } else {
                throw new IllegalStateException(
                  "not enough arguments to unpack"
                )
              }
            })
            cursors.zip(old).foreach(x => context.setVar(x._1, x._2))
        }
      case map(m) =>
        ctx.ident.size match {
          case 2 =>
            val cursorKeyName = visit(ctx.ident(0)).toString
            val cursorValueName = visit(ctx.ident(1)).toString
            val oldKey = context.getVar(cursorKeyName)
            val oldValue = context.getVar(cursorValueName)
            m.foreach(el => {
              context.setVar(cursorKeyName, el._1)
              context.setVar(cursorValueName, el._2)
              visit(ctx.block)
            })
            context.setVar(cursorKeyName, oldKey)
            context.setVar(cursorValueName, oldValue)
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val oldCursor = context.getVar(cursorName)
            m.foreach(el => {
              context.setVar(cursorName, el._2)
              visit(ctx.block)
            })
            context.setVar(cursorName, oldCursor)
          case _ =>
            throw new IllegalStateException("too many cursors for map")
        }
      case other =>
        throw new IllegalArgumentException("cursor must be collection")
    }
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
