package org.grenki.gsql.visitor

import org.antlr.v4.runtime.misc.Interval
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.sqlParser

import scala.collection.mutable

trait LiteralVisitor extends BaseVisitor {

  override def visitAny_char(ctx: sqlParser.Any_charContext): Type = {
    val q = tokenStream.getText(new Interval(ctx.start.getTokenIndex - 2, ctx.start.getTokenIndex - 2))
    val pre = tokenStream.getText(new Interval(ctx.start.getTokenIndex - 1, ctx.start.getTokenIndex - 1))
    val post = tokenStream.getText(new Interval(ctx.stop.getTokenIndex + 1, ctx.stop.getTokenIndex + 1))
    val str = tokenStream.getText(new Interval(ctx.start.getTokenIndex, ctx.start.getTokenIndex))
    var res = str
    if ((pre.startsWith(" ") || pre.startsWith("\n") || pre.startsWith("\t")) && (q == "'" || q == "\"" || q == "`")) {
      res = pre + res;
    }
    if (post.startsWith(" ") || post.startsWith("\n") || post.startsWith("\t")) {
      res = res + post;
    }
    string(res)
  }

  override def visitString(ctx: sqlParser.StringContext): Type = {
    val s = new mutable.StringBuilder();
    for (i <- 1 until ctx.getChildCount - 1) {
      val value = visit(ctx.getChild(i))
      s.append(value)
    }
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
