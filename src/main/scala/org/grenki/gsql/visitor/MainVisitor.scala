package org.grenki.gsql.visitor

import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.{Type, string, Null}
import org.grenki.gsql.sql

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval

class MainVisitor(ctx: Context, tokens: TokenStream)
  extends ExpressionVisitor
    with LiteralVisitor
    with ControlStmtsVisitor {

  val context = ctx
  val tokenStream = tokens

  override def visitAny_comma(ctx: sql.Any_commaContext): Type = {
    context.execute(visit(ctx.other()).toString)
  }

  override def visitAssigment_stmt(ctx: sql.Assigment_stmtContext): Type = {
    context.setVar(visit(ctx.ident()).toString, visit(ctx.expr()))
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
        case other => other.toString
      }
      res += tokenStream.getText(new Interval(from, to)) + ch
      from = child.getSourceInterval.b + 1
    })
    string(res)
  }

  override def visitVar(ctx: sql.VarContext): Type = {
    context.getVar(visit(ctx.ident()).toString)
  }

  override def visitInterpolation_exp(ctx: sql.Interpolation_expContext): Type = {
    visit(ctx.expr())
  }

  override def visitPrint_stmt(ctx: sql.Print_stmtContext): Type = {
    println("[USER PRINT]: " + visit(ctx.expr()).toString)
    Null
  }
}
