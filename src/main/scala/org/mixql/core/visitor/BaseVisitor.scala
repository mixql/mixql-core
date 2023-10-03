package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.mixql.core.context.{Context, ControlContext}
import org.mixql.core.context.mtype._
import org.mixql.core.generated.{sqlBaseVisitor, token}

import scala.language.implicitConversions

trait BaseVisitor extends sqlBaseVisitor[MType] {
  val context: Context
  val tokenStream: TokenStream
  var controlState: ControlContext.ControlContext

  protected implicit def is_ctx_defined(rule: ParseTree): Boolean = rule != null

  protected implicit def to_bool(base: MType): Boolean = {
    base match {
      case ok: MBool => ok.getValue
      case other: MType =>
        throw new IllegalArgumentException(
          s"type mismatch: condition bool expected but got ${other.getClass.getSimpleName}"
        )
    }
  }

  override def visitTerminal(node: TerminalNode): MType = {
    node.getSymbol().getType() match {
      case token.T_ESCAPED_SYMBOLS =>
        new MString(node.getText() match {
          case "\\$"    => "$"
          case "\\\\"   => """\"""
          case "\\;"    => ";"
          case """\n""" => """\n"""
          case """\r""" => """\r"""
          case """\t""" => """\t"""
        })
      case token.T_SS_ESC =>
        new MString(node.getText() match {
          case "\\\\n"  => """\n"""
          case "\\\\r"  => """\r"""
          case "\\\\t"  => """\t"""
          case "\\\\"   => """\"""
          case "\\$"    => "$"
          case "\n"     => "\n"
          case """\n""" => "\n"
          case """\r""" => "\r"
          case "\r"     => "\r"
          case """\t""" => "\t"
          case "\t"     => "\t"
          case """\'""" => """'"""
        })
      case token.T_DS_ESC =>
        new MString(node.getText() match {
          case "\\\\n"  => """\n"""
          case "\\\\r"  => """\r"""
          case "\\\\t"  => """\t"""
          case "\\\\"   => """\"""
          case "\\$"    => "$"
          case "\n"     => "\n"
          case """\n""" => "\n"
          case """\r""" => "\r"
          case "\r"     => "\r"
          case """\t""" => "\t"
          case "\t"     => "\t"
          case """\"""" => """""""
        })
      case token.T_BS_ESC =>
        new MString(node.getText() match {
          case "\\\\n"  => """\n"""
          case "\\\\r"  => """\r"""
          case "\\\\t"  => """\t"""
          case "\\\\"   => """\"""
          case "\\$"    => "$"
          case "\n"     => "\n"
          case """\n""" => "\n"
          case """\r""" => "\r"
          case "\r"     => "\r"
          case """\t""" => "\t"
          case "\t"     => "\t"
          case """\`""" => """`"""
        })
      case token.T_SS_VAR_INTERPOLATION => context.getVar(node.getText().substring(1))
      case token.T_DS_VAR_INTERPOLATION => context.getVar(node.getText().substring(1))
      case token.T_BS_VAR_INTERPOLATION => context.getVar(node.getText().substring(1))
      case _                            => new MString(node.getText())
    }
  }

  override def defaultResult(): MType = MNull.get()

  override def aggregateResult(aggregate: MType, nextResult: MType): MType = nextResult
}
