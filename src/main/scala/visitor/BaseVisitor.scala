package org.grenki.gsql
package visitor

import context.Context
import context.`type`.{Type, bool, string, void}

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.tree.TerminalNode

trait BaseVisitor extends sqlBaseVisitor[Type] {
  val context: Context[Type]
  val tokenStream: TokenStream

  protected implicit def to_bool(base: Type): Boolean = {
    base match {
      case ok: bool => ok.value
      case other: Type => throw new IllegalArgumentException(s"type mismatch: condition must be bool but got ${other.getClass.getSimpleName}")
    }
  }

  override def visitTerminal(node: TerminalNode): Type =
    string(node.getText)

  override def defaultResult(): Type = void

  override def aggregateResult(aggregate: Type, nextResult: Type): Type =
    nextResult

}
