package org.mixql.core.visitor

import org.mixql.core.context.mtype._
import org.mixql.core.generated.sql

import java.time.LocalDateTime
import java.time.LocalDate
import scala.collection.mutable.{Map => MutMap}
import scala.collection.JavaConverters._

trait LiteralVisitor extends BaseVisitor {
  override def visitSingle_quotedString(ctx: sql.Single_quotedStringContext): MType = visit(ctx.s_string)

  override def visitSlash_quotedString(ctx: sql.Slash_quotedStringContext): MType = visit(ctx.b_string)

  override def visitDouble_quotedString(ctx: sql.Double_quotedStringContext): MType = visit(ctx.d_string)

  override def visitS_string(ctx: sql.S_stringContext): MType = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new MString(res, "'")
  }

  override def visitS_interpolation_expr(ctx: sql.S_interpolation_exprContext): MType = visit(ctx.expr)

  override def visitB_string(ctx: sql.B_stringContext): MType = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new MString(res, "`")
  }

  override def visitB_interpolation_expr(ctx: sql.B_interpolation_exprContext): MType = visit(ctx.expr)

  override def visitD_string(ctx: sql.D_stringContext): MType = {
    var res = ""
    if (ctx.children != null)
      ctx.children.forEach(ch => res += visit(ch).toString)
    new MString(res, "\"")
  }

  override def visitD_interpolation_expr(ctx: sql.D_interpolation_exprContext): MType = visit(ctx.expr)

  override def visitLiteral_string(ctx: sql.Literal_stringContext): MType = visit(ctx.string)

  override def visitLiteral_int(ctx: sql.Literal_intContext): MType =
    if (ctx.int_number.T_SUB)
      new MInt(-ctx.int_number.L_INT.getText.toLong)
    else
      new MInt(ctx.int_number.L_INT.getText.toLong)

  override def visitLiteral_double(ctx: sql.Literal_doubleContext): MType =
    if (ctx.dec_number.T_SUB)
      new MDouble(-ctx.dec_number.L_DEC.getText.toDouble)
    else
      new MDouble(ctx.dec_number.L_DEC.getText.toDouble)

  override def visitLiteral_bool(ctx: sql.Literal_boolContext): MType =
    if (ctx.bool_literal.T_FALSE)
      new MBool(false)
    else if (ctx.bool_literal.T_TRUE)
      new MBool(true)
    else
      throw new IllegalArgumentException("unknown bool literal")

  override def visitLiteral_null(ctx: sql.Literal_nullContext): MType = MNull.get()

  override def visitLiteral_none(ctx: sql.Literal_noneContext): MType = MNone.get()

  override def visitLiteral_current_date(ctx: sql.Literal_current_dateContext): MType = {
    new MString(LocalDate.now().toString)
  }

  override def visitLiteral_current_timestamp(ctx: sql.Literal_current_timestampContext): MType = {
    new MString(LocalDateTime.now().toString)
  }

  override def visitLiteral_array(ctx: sql.Literal_arrayContext): MType = {
    new MArray(ctx.array_literal.expr.asScala.map(visit).toArray)
  }

  override def visitLiteral_map(ctx: sql.Literal_mapContext): MType = {
    val res =
      ctx.map_literal.map_item.asScala.map(item => {
        val k = visit(item.key)
        if (k.isInstanceOf[MCollection])
          throw new IllegalArgumentException("collection can't be key")
        k -> visit(item.value)
      }).toMap
    import scala.collection.JavaConverters._
    new MMap(scala.collection.mutable.Map(res.toSeq: _*).asJava)
  }
}
