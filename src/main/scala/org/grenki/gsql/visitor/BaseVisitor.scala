package org.grenki.gsql.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.tree.{TerminalNode, ParseTree}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype._
import org.grenki.gsql.{sqlBaseVisitor, token}

import scala.language.implicitConversions

trait BaseVisitor extends sqlBaseVisitor[Type] {
  val context: Context
  val tokenStream: TokenStream

  protected implicit def is_ctx_defined(rule: ParseTree): Boolean =
    rule != null

  protected implicit def to_bool(base: Type): Boolean = {
    base match {
      case ok: bool => ok.value
      case other: Type =>
        throw new IllegalArgumentException(
          s"type mismatch: condition bool expected but got ${other.getClass.getSimpleName}"
        )
    }
  }

  override def visitTerminal(node: TerminalNode): Type = {
    node.getSymbol().getType() match {
      case token.T_ESCAPED_SYMBOLS => string(node.getText().substring(1))
      case token.T_SS_ESC          => string(node.getText().substring(1))
      case token.T_DS_ESC          => string(node.getText().substring(1))
      case token.T_BS_ESC          => string(node.getText().substring(1))
      case token.T_SS_VAR_INTERPOLATION =>
        context.getVar(node.getText().substring(1))
      case token.T_DS_VAR_INTERPOLATION =>
        context.getVar(node.getText().substring(1))
      case token.T_BS_VAR_INTERPOLATION =>
        context.getVar(node.getText().substring(1))
      case _ => string(node.getText())
    }
  }

  override def defaultResult(): Type = Null

  override def aggregateResult(aggregate: Type, nextResult: Type): Type =
    nextResult

}
