package org.grenki.gsql.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.misc.Interval
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.{Type, int, string, void}
import org.grenki.gsql.function.FunctionInvoker
import org.grenki.gsql.sqlParser

import scala.jdk.CollectionConverters._

class MainVisitor(ctx: Context[Type], tokens: TokenStream)
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

  override def visitExpr_func(ctx: sqlParser.Expr_funcContext): Type = {
    val func = ctx.func()
    val ident = func.ident(0)
    val funcName = tokenStream.getText(new Interval(ident.start.getTokenIndex, ident.stop.getTokenIndex))
    //todo: add implicit cast
    val params: Seq[Any] = func.expr().asScala.map(visit(_)).map {
      case string(v) => v
      case int(v)=>v
    }.toSeq
    FunctionInvoker.invoke(context.functions.toMap, funcName, params) match {
      case p:String => string(p)
      case p:Int => int(p)
    }
  }
}
