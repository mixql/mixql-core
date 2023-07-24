package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.mixql.core.context.{Context, ControlContext}
import org.mixql.core.context.gtype._
import org.mixql.core.generated.sql

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.util.control.Breaks.{break, _}
import org.mixql.core.exception.UserSqlException

trait ControlStmtsVisitor extends BaseVisitor {
  def toUserSqlException(e: Throwable): UserSqlException = {
    if (e.isInstanceOf[UserSqlException])
      return e.asInstanceOf[UserSqlException]
    return new UserSqlException(e.getClass.getSimpleName, e.getMessage)
  }

  override def visitTry_catch_stmt(ctx: sql.Try_catch_stmtContext): Type = {
    try {
      val block = visit(ctx.try_bock)
      if (controlState == ControlContext.RETURN)
        return block
    } catch {
      case e: Throwable =>
        val exception = toUserSqlException(e)
        val block =
          if (ctx.exc) {
            val old_exc = context.getVar(visit(ctx.exc).toString)
            val old_message = context
              .getVar(visit(ctx.exc).toString + ".message")
            context
              .setVar(visit(ctx.exc).toString, new string(exception.exc_type))
            context.setVar(visit(ctx.exc).toString + ".message",
                           new string(exception.detailMessage))
            val block = visit(ctx.catch_block)
            context.setVar(visit(ctx.exc).toString, old_exc)
            context.setVar(visit(ctx.exc).toString + ".message", old_message)
            block
          } else {
            visit(ctx.catch_block)
          }
        if (controlState == ControlContext.RETURN)
          return block
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
    var result: Type = new Null
    val i_name = visit(ctx.ident).toString
    val old = context.getVar(i_name)
    var i =
      if (!ctx.T_REVERSE)
        visit(ctx.from)
      else
        visit(ctx.to)
    val to =
      if (!ctx.T_REVERSE)
        visit(ctx.to)
      else
        visit(ctx.from)
    val step =
      (
        if (ctx.T_REVERSE)
          new gInt(-1)
        else
          new gInt(1)
      ).Multiply(
        if (ctx.step)
          visit(ctx.step)
        else
          new gInt(1))
    context.setVar(i_name, i)
    breakable {
      while ((!ctx.T_REVERSE && i.LessThen(to)) ||
             (ctx.T_REVERSE && i.MoreThen(to))) {
        val block = visit(ctx.block)
        if (controlState == ControlContext.RETURN) {
          context.setVar(i_name, old)
          return block
        }
        if (controlState == ControlContext.BREAK) {
          controlState = ControlContext.NONE
          break
        }
        i = i.Add(step)
        context.setVar(i_name, i)
      }
    }
    if ((!ctx.T_REVERSE && i.MoreEqualThen(to)) ||
        (ctx.T_REVERSE && i.LessEqualThen(to))) {
      context.setVar(i_name, to)
      visit(ctx.block)
    }
    context.setVar(i_name, old)
    result
  }

  def execForInGcursor(cursor: gcursor,
                       ctx: sql.For_cursor_stmtContext): Type = {
    cursor.open()

    var fetchRes = cursor.fetch()
    while (!fetchRes.isInstanceOf[nothing]) {
//      fetchRes match {
//        case collection1: collection => execBlockInFor(collection1, ctx)
//        case _ => execFetchBlockInFor(fetchRes, ctx)
//      }
      execFetchBlockInFor(fetchRes, ctx)
      fetchRes = cursor.fetch()
    }
    new Null
  }

  def execFetchBlockInFor(inRes: Type,
                          ctx: sql.For_cursor_stmtContext): Type = {
    var result: Type = new Null
    ctx.ident.size match {
      case 1 =>
        val cursorName = visit(ctx.ident(0)).toString
        val old = context.getVar(cursorName)
        breakable {
          context.setVar(cursorName, inRes)
          val block = visit(ctx.block)
          if (controlState == ControlContext.RETURN) {
            result = block
            break
          }
          if (controlState == ControlContext.BREAK) {
            controlState = ControlContext.NONE
            break
          }
        }
        context.setVar(cursorName, old)
      case other =>
        throw new IllegalStateException(
          "too many arguments to unpack result from cursor, which did not return collection")
    }
    result
  }

  def execBlockInFor(inRes: collection,
                     ctx: sql.For_cursor_stmtContext): Type = {
    var result: Type = new Null
    inRes match {
      case c: array =>
        ctx.ident.size match {
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val old = context.getVar(cursorName)
            breakable {
              c.getArr
                .foreach(el => {
                  context.setVar(cursorName, el)
                  val block = visit(ctx.block)
                  if (controlState == ControlContext.RETURN) {
                    result = block
                    break
                  }
                  if (controlState == ControlContext.BREAK) {
                    controlState = ControlContext.NONE
                    break
                  }
                })
            }
            context.setVar(cursorName, old)
          case other =>
            val cursors = ctx.ident.asScala.map(visit(_).toString).toList
            val old = cursors.map(context.getVar(_))
            breakable {
              c.getArr
                .foreach(el => {
                  if (el.isInstanceOf[array]) {
                    val a = el.asInstanceOf[array]
                    if (a.getArr.size < cursors.size)
                      throw new IllegalStateException(
                        "not enough arguments to unpack")
                    cursors
                      .zip(a.getArr)
                      .foreach(kv => {
                        context.setVar(kv._1, kv._2)
                      })
                    val block = visit(ctx.block)
                    if (controlState == ControlContext.RETURN) {
                      result = block
                      break
                    }
                    if (controlState == ControlContext.BREAK) {
                      controlState = ControlContext.NONE
                      break
                    }
                  } else {
                    throw new IllegalStateException(
                      "not enough arguments to unpack")
                  }
                })
            }
            cursors.zip(old).foreach(x => context.setVar(x._1, x._2))
        }
      case c: map =>
        ctx.ident.size match {
          case 2 =>
            val cursorKeyName = visit(ctx.ident(0)).toString
            val cursorValueName = visit(ctx.ident(1)).toString
            val oldKey = context.getVar(cursorKeyName)
            val oldValue = context.getVar(cursorValueName)
            breakable {
              c.getMap
                .foreach(el => {
                  context.setVar(cursorKeyName, el._1)
                  context.setVar(cursorValueName, el._2)
                  val block = visit(ctx.block)
                  if (controlState == ControlContext.RETURN) {
                    result = block
                    break
                  }
                  if (controlState == ControlContext.BREAK) {
                    controlState = ControlContext.NONE
                    break
                  }
                })
            }
            context.setVar(cursorKeyName, oldKey)
            context.setVar(cursorValueName, oldValue)
          case 1 =>
            val cursorName = visit(ctx.ident(0)).toString
            val oldCursor = context.getVar(cursorName)
            breakable {
              c.getMap
                .foreach(el => {
                  context.setVar(cursorName, el._2)
                  val block = visit(ctx.block)
                  if (controlState == ControlContext.RETURN) {
                    result = block
                    break
                  }
                  if (controlState == ControlContext.BREAK) {
                    controlState = ControlContext.NONE
                    break
                  }
                })
            }
            context.setVar(cursorName, oldCursor)
          case _ =>
            throw new IllegalStateException("too many cursors for map")
        }
      case other =>
        throw new IllegalArgumentException("cursor must be collection")
    }
    result
  }

  override def visitWhile_stmt(ctx: sql.While_stmtContext): Type = {
    var condition: Boolean = visit(ctx.expr)
    breakable {
      while (condition) {
        val block = visit(ctx.block)
        if (controlState == ControlContext.RETURN)
          return block
        if (controlState == ControlContext.BREAK) {
          controlState = ControlContext.NONE
          break
        }
        condition = visit(ctx.expr)
      }
    }
    new Null()
  }
}
