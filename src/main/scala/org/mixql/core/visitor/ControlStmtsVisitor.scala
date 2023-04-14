package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.generated.sql

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import scala.collection.convert.ImplicitConversions.`map AsScala`

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
            new string(e.getClass.getSimpleName)
          )
        }
        if (old_message.nonEmpty) {
          context.setVar(
            visit(ctx.exc).toString + ".message",
            new string(e.getMessage)
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
    new Null()
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
        new Null()
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
      (if (ctx.T_REVERSE) new gInt(-1) else new gInt(1)).Multiply(if (ctx.step) visit(ctx.step)
                                                  else new gInt(1))
    context.setVar(i_name, i)
    while (
      (!ctx.T_REVERSE && i.LessThen(to)) ||
      (ctx.T_REVERSE && i.MoreThen(to))
    ) {
      visit(ctx.block)
      i = i.Add(step)
      context.setVar(i_name, i)
    }
    if (
      (!ctx.T_REVERSE && i.MoreEqualThen(to)) ||
      (ctx.T_REVERSE && i.LessEqualThen(to))
    ) {
      context.setVar(i_name, to)
      visit(ctx.block)
    }
    context.setVar(i_name, old)
    new Null()
  }

  override def visitFor_cursor_stmt(ctx: sql.For_cursor_stmtContext): Type = {
    val cursor = visit(ctx.expr)
    cursor match {
      case c: array =>
        ctx.ident.size match {
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val old = context.getVar(cursorName)
            c.getArr.foreach(el => {
              context.setVar(cursorName, el)
              visit(ctx.block)
            })
            context.setVar(cursorName, old)
          case other =>
            val cursors = ctx.ident.asScala.map(visit(_).toString).toList
            val old = cursors.map(context.getVar(_))
            c.getArr.foreach(el => {
              if (el.isInstanceOf[array]) {
                val a = el.asInstanceOf[array]
                if (a.getArr.size < cursors.size)
                  throw new IllegalStateException(
                    "not enough arguments to unpack"
                  )
                cursors
                  .zip(a.getArr)
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
      case c: map =>
        ctx.ident.size match {
          case 2 =>
            val cursorKeyName = visit(ctx.ident(0)).toString
            val cursorValueName = visit(ctx.ident(1)).toString
            val oldKey = context.getVar(cursorKeyName)
            val oldValue = context.getVar(cursorValueName)
            c.getMap.foreach(el => {
              context.setVar(cursorKeyName, el._1)
              context.setVar(cursorValueName, el._2)
              visit(ctx.block)
            })
            context.setVar(cursorKeyName, oldKey)
            context.setVar(cursorValueName, oldValue)
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val oldCursor = context.getVar(cursorName)
            c.getMap.foreach(el => {
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
    new Null()
  }

  override def visitWhile_stmt(ctx: sql.While_stmtContext): Type = {
    var condition: Boolean = visit(ctx.expr)
    while (condition) {
      visit(ctx.block)
      condition = visit(ctx.expr)
    }
    new Null()
  }
}
