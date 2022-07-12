package org.grenki.gsql
package visitor

import context.`type`.{Type, int, void}

import scala.language.implicitConversions

trait ControlStmtsVisitor extends BaseVisitor {

  override def visitIf_stmt(ctx: sqlParser.If_stmtContext): Type = {
    val condition: Boolean = visit(ctx.expr())
    if (condition) {
      visit(ctx.block())
      return void
    } else {
      ctx.elseif_block().forEach(elif => {
        val elsecondition: Boolean = visit(elif.expr())
        if (elsecondition) {
          visit(elif.block())
          return void
        }
      })
      if (ctx.else_block() != null) {
        visit(ctx.else_block().block())
        return void
      }
    }
    void
  }

  // TODO maybe better realisation using for?
  override def visitFor_range_stmt(ctx: sqlParser.For_range_stmtContext): Type = {
    //super.visitFor_range_stmt(ctx)
    val i_name = visit(ctx.ident()).toString
    val old = context.vars.getOrElse(i_name, void)
    var i = visit(ctx.from)
    val to = visit(ctx.to)
    val step = (if (ctx.T_REVERSE() != null) int(-1) else int(1)) * (if (ctx.step != null) visit(ctx.step) else int(1))
    context.setVar(i_name, i)
    while (i < to) {
      visit(ctx.block())
      i = i + step
      context.setVar(i_name, i)
    }
    if (i >= to) {
      context.setVar(i_name, to)
      visit(ctx.block())
    }
    context.setVar(i_name, old)
    void
  }

  override def visitWhile_stmt(ctx: sqlParser.While_stmtContext): Type = {
    var condition: Boolean = visit(ctx.expr())
    while (condition) {
      visit(ctx.block())
      condition = visit(ctx.expr())
    }
    void
  }
}
