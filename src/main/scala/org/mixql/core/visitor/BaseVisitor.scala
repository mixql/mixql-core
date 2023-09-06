package org.mixql.core.visitor

import org.antlr.v4.runtime.TokenStream
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.mixql.core.context.{Context, ControlContext}
import org.mixql.core.context.gtype._
import org.mixql.core.generated.{sqlBaseVisitor, token}

import scala.language.implicitConversions

trait BaseVisitor extends sqlBaseVisitor[Type] {
  val context: Context
  val tokenStream: TokenStream
  var controlState: ControlContext.ControlContext

  protected implicit def is_ctx_defined(rule: ParseTree): Boolean = rule != null

  protected implicit def to_bool(base: Type): Boolean = {
    base match {
      case ok: bool => ok.getValue
      case other: Type =>
        throw new IllegalArgumentException(
          s"type mismatch: condition bool expected but got ${other.getClass.getSimpleName}"
        )
    }
  }

  override def visitTerminal(node: TerminalNode): Type = {
    node.getSymbol().getType() match {
      case token.T_ESCAPED_SYMBOLS =>
        new string(node.getText() match {
          case "\\$"    => "$"
          case "\\\\"   => """\"""
          case "\\;"    => ";"
          case """\n""" => """\n"""
          case """\r""" => """\r"""
          case """\t""" => """\t"""
        })
      case token.T_SS_ESC =>
        new string(node.getText() match {
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
        new string(node.getText() match {
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
        new string(node.getText() match {
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
      case _                            => new string(node.getText())
    }
  }

  override def defaultResult(): Type = new Null()

  override def aggregateResult(aggregate: Type, nextResult: Type): Type = nextResult
}
