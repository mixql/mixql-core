package org.grenki.gsql.test

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.context.gtype.Type
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.{sqlLexer, sqlParser}
import org.scalatest.funsuite.AnyFunSuite

class MainVisitorBaseTest extends AnyFunSuite{
  def runMainVisitor(code: String, context: Context[Type] = new Context[Type]()): Context[Type] = {
    val lexer = new sqlLexer(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(new sqlLexer(CharStreams.fromString(code)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sqlParser(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }
}
