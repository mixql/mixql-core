package org.mixql.core.visitor

import org.mixql.core.context.gtype._
import org.mixql.core.generated.sql

import java.time.LocalDateTime
import java.time.LocalDate
import scala.collection.mutable.{Map => MutMap}
import scala.collection.JavaConverters._

trait LiteralVisitor extends BaseVisitor {
  override def visitSingle_quotedString(ctx: sql.Single_quotedStringContext): Type = visit(ctx.s_string)

  override def visitSlash_quotedString(ctx: sql.Slash_quotedStringContext): Type = visit(ctx.b_string)

  override def visitDouble_quotedString(ctx: sql.Double_quotedStringContext): Type = visit(ctx.d_string)

  override def visitS_string(ctx: sql.S_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new string(res, "'")
  }

  override def visitS_interpolation_expr(ctx: sql.S_interpolation_exprContext): Type = visit(ctx.expr)

  override def visitB_string(ctx: sql.B_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new string(res, "`")
  }

  override def visitB_interpolation_expr(ctx: sql.B_interpolation_exprContext): Type = visit(ctx.expr)

  override def visitD_string(ctx: sql.D_stringContext): Type = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new string(res, "\"")
  }

  override def visitD_interpolation_expr(ctx: sql.D_interpolation_exprContext): Type = visit(ctx.expr)

  override def visitLiteral_string(ctx: sql.Literal_stringContext): Type = visit(ctx.string)

  override def visitLiteral_int(ctx: sql.Literal_intContext): Type =
    if (ctx.int_number.T_SUB)
      new gInt(-ctx.int_number.L_INT.getText.toLong)
    else
      new gInt(ctx.int_number.L_INT.getText.toLong)

  override def visitLiteral_double(ctx: sql.Literal_doubleContext): Type =
    if (ctx.dec_number.T_SUB)
      new gDouble(-ctx.dec_number.L_DEC.getText.toDouble)
    else
      new gDouble(ctx.dec_number.L_DEC.getText.toDouble)

  override def visitLiteral_bool(ctx: sql.Literal_boolContext): Type =
    if (ctx.bool_literal.T_FALSE)
      new bool(false)
    else if (ctx.bool_literal.T_TRUE)
      new bool(true)
    else
      throw new IllegalArgumentException("unknown bool literal")

  override def visitLiteral_null(ctx: sql.Literal_nullContext): Type = new Null()

  override def visitLiteral_none(ctx: sql.Literal_noneContext): Type = new none()

  override def visitLiteral_current_date(ctx: sql.Literal_current_dateContext): Type = {
    new string(LocalDate.now().toString)
  }

  override def visitLiteral_current_timestamp(ctx: sql.Literal_current_timestampContext): Type = {
    new string(LocalDateTime.now().toString)
  }

  override def visitLiteral_array(ctx: sql.Literal_arrayContext): Type = {
    new array(ctx.array_literal.expr.asScala.map(visit).toArray)
  }

  override def visitLiteral_map(ctx: sql.Literal_mapContext): Type = {
    val res =
      ctx.map_literal.map_item.asScala.map(item => {
        val k = visit(item.key)
        if (k.isInstanceOf[collection])
          throw new IllegalArgumentException("collection can't be key")
        k -> visit(item.value)
      }).toMap
    import scala.collection.JavaConverters._
    new map(scala.collection.mutable.Map(res.toSeq: _*).asJava)
  }
}
