package org.grenki.gsql.visitor

import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.{Type, string, void}
import org.grenki.gsql.sqlParser

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval

class MainVisitor(ctx: Context, tokens: TokenStream)
  extends ExpressionVisitor
    with LiteralVisitor
    with ControlStmtsVisitor {

  val context = ctx
  val tokenStream = tokens

  override def visitAny_comma(ctx: sqlParser.Any_commaContext): Type = {
    context.execute(visit(ctx.any()).toString)
    void
  }

  override def visitAssigment_stmt(ctx: sqlParser.Assigment_stmtContext): Type = {
    context.setVar(visit(ctx.ident()).toString, visit(ctx.expr()))
    void
  }

  override def visitAny(ctx: sqlParser.AnyContext): Type = {
    var res = ""
    var from = ctx.start.getTokenIndex
    var to = from
    ctx.children.forEach(child => {
      to = child.getSourceInterval.a - 1
      res += tokenStream.getText(new Interval(from, to)) + visit(child)
      from = child.getSourceInterval.b + 1
    })
    string(res)
  }

  override def visitVar(ctx: sqlParser.VarContext): Type = {
    context.getVar(visit(ctx.ident()).toString)
  }

  override def visitInterpolation_exp(ctx: sqlParser.Interpolation_expContext): Type = {
    visit(ctx.expr())
  }

  override def visitPrint_stmt(ctx: sqlParser.Print_stmtContext): Type = {
    println("[USER PRINT]: " + visit(ctx.expr()).toString)
    void
  }
}
