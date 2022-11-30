package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.sql
import java.time.LocalDateTime
import java.time.LocalDate

import scala.jdk.CollectionConverters._

trait LiteralVisitor extends BaseVisitor {
  override def visitSingle_quotedString(
    ctx: sql.Single_quotedStringContext
  ): Type =
    visit(ctx.s_string)

  override def visitSlash_quotedString(
    ctx: sql.Slash_quotedStringContext
  ): Type =
    visit(ctx.b_string)

  override def visitDouble_quotedString(
    ctx: sql.Double_quotedStringContext
  ): Type =
    visit(ctx.d_string)

  override def visitS_string(ctx: sql.S_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    string(res, "'")
  }

  override def visitS_interpolation_expr(
    ctx: sql.S_interpolation_exprContext
  ): Type =
    visit(ctx.expr)

  override def visitB_string(ctx: sql.B_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    string(res, "`")
  }

  override def visitB_interpolation_expr(
    ctx: sql.B_interpolation_exprContext
  ): Type =
    visit(ctx.expr)

  override def visitD_string(ctx: sql.D_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    string(res, "\"")
  }

  override def visitD_interpolation_expr(
    ctx: sql.D_interpolation_exprContext
  ): Type =
    visit(ctx.expr)

  override def visitLiteral_string(ctx: sql.Literal_stringContext): Type =
    visit(ctx.string)

  override def visitLiteral_int(ctx: sql.Literal_intContext): Type =
    if (ctx.int_number.T_SUB)
      int(-ctx.int_number.L_INT.getText.toInt)
    else
      int(ctx.int_number.L_INT.getText.toInt)

  override def visitLiteral_double(ctx: sql.Literal_doubleContext): Type =
    if (ctx.dec_number.T_SUB)
      double(-ctx.dec_number.L_DEC.getText.toDouble)
    else
      double(ctx.dec_number.L_DEC.getText.toDouble)

  override def visitLiteral_bool(ctx: sql.Literal_boolContext): Type =
    if (ctx.bool_literal.T_FALSE)
      bool(false)
    else if (ctx.bool_literal.T_TRUE)
      bool(true)
    else
      throw new IllegalArgumentException("unknown bool literal")

  override def visitLiteral_null(ctx: sql.Literal_nullContext): Type =
    Null

  override def visitLiteral_current_date(
    ctx: sql.Literal_current_dateContext
  ): Type = {
    string(LocalDate.now().toString)
  }

  override def visitLiteral_current_timestamp(
    ctx: sql.Literal_current_timestampContext
  ): Type = {
    string(LocalDateTime.now().toString)
  }

  override def visitLiteral_array(ctx: sql.Literal_arrayContext): Type = {
    array(ctx.array_literal.expr.asScala.map(visit).toArray)
  }
}
