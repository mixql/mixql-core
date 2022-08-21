package org.grenki.gsql.visitor

import org.grenki.gsql.context.gtype._
import org.grenki.gsql.sqlParser

import scala.collection.mutable

trait LiteralVisitor extends BaseVisitor {

  override def visitString(ctx: sqlParser.StringContext): Type = {
    val s = new mutable.StringBuilder();
    for (i <- 1 until ctx.getChildCount - 1)
      s.append(visit(ctx.getChild(i)))
    string(s.toString())
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
