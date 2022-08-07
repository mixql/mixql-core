package org.grenki.gsql.visitor

import org.grenki.gsql.context.gtype._
import org.grenki.gsql.sqlParser

import org.antlr.v4.runtime.misc.Interval

trait LiteralVisitor extends BaseVisitor {
  override def visitString(ctx: sqlParser.StringContext): Type = {
    val pre = tokenStream.getText(new Interval(ctx.start.getTokenIndex + 1, ctx.any().start.getTokenIndex - 1))
    val post = tokenStream.getText(new Interval(ctx.any().stop.getTokenIndex + 1, ctx.stop.getTokenIndex - 1))
    string(pre + visit(ctx.any()) + post)
  }

  override def visitLiteral_string(ctx: sqlParser.Literal_stringContext): Type =
    visit(ctx.string())

  override def visitLiteral_int(ctx: sqlParser.Literal_intContext): Type =
    if (ctx.int_number().T_SUB() != null)
      int(-ctx.int_number().L_INT().getText.toInt)
    else
      int(ctx.int_number().L_INT().getText.toInt)

  override def visitLiteral_double(ctx: sqlParser.Literal_doubleContext): Type =
    if (ctx.dec_number().T_SUB() != null)
      double(-ctx.dec_number().L_DEC().getText.toDouble)
    else
      double(ctx.dec_number().L_DEC().getText.toDouble)

  override def visitLiteral_bool(ctx: sqlParser.Literal_boolContext): Type =
    if (ctx.bool_literal().T_FALSE() != null)
      bool(false)
    else if (ctx.bool_literal().T_TRUE() != null)
      bool(true)
    else
      throw new IllegalArgumentException("unknown bool literal")

  override def visitLiteral_null(ctx: sqlParser.Literal_nullContext): Type = 
    void
}
