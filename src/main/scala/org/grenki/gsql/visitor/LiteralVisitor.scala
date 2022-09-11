package org.grenki.gsql.visitor

import org.antlr.v4.runtime.misc.Interval
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.sql

import scala.collection.mutable

trait LiteralVisitor extends BaseVisitor {

  override def visitSingle_quotedString(ctx: sql.Single_quotedStringContext): Type =
    visit(ctx.s_string)

  override def visitSlash_quotedString(ctx: sql.Slash_quotedStringContext): Type =
    visit(ctx.b_string)

  override def visitDouble_quotedString(ctx: sql.Double_quotedStringContext): Type =
    visit(ctx.d_string)

  override def visitS_string(ctx: sql.S_stringContext): Type = {
    var res = ""
    if (ctx.children != null) ctx.children.forEach(ch =>
      res += visit(ch).toString
    )
    string(res, "'")
  }
  override def visitS_interpolation_exp(ctx: sql.S_interpolation_expContext): Type = 
    visit(ctx.expr())

  override def visitB_string(ctx: sql.B_stringContext): Type = {
    var res = ""
    if (ctx.children != null) ctx.children.forEach(ch =>
      res += visit(ch).toString
    )
    string(res, "`")
  }
  override def visitB_interpolation_exp(ctx: sql.B_interpolation_expContext): Type =
    visit(ctx.expr())

  override def visitD_string(ctx: sql.D_stringContext): Type = {
    var res = ""
    if (ctx.children != null) ctx.children.forEach(ch =>
      res += visit(ch).toString
    )
    string(res, "\"")
  }
  override def visitD_interpolation_exp(ctx: sql.D_interpolation_expContext): Type =
    visit(ctx.expr())

  override def visitLiteral_string(ctx: sql.Literal_stringContext): Type =
    visit(ctx.string())

  override def visitLiteral_int(ctx: sql.Literal_intContext): Type =
    if (ctx.int_number().T_SUB() != null)
      int(-ctx.int_number().L_INT().getText.toInt)
    else
      int(ctx.int_number().L_INT().getText.toInt)

  override def visitLiteral_double(ctx: sql.Literal_doubleContext): Type =
    if (ctx.dec_number().T_SUB() != null)
      double(-ctx.dec_number().L_DEC().getText.toDouble)
    else
      double(ctx.dec_number().L_DEC().getText.toDouble)

  override def visitLiteral_bool(ctx: sql.Literal_boolContext): Type =
    if (ctx.bool_literal().T_FALSE() != null)
      bool(false)
    else if (ctx.bool_literal().T_TRUE() != null)
      bool(true)
    else
      throw new IllegalArgumentException("unknown bool literal")

  override def visitLiteral_null(ctx: sql.Literal_nullContext): Type =
    Null
}
