package org.grenki.gsql

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.token

package object gsql {
  def run(script: String, context: Context): Unit = {
    val lexer = new token(CharStreams.fromString(script))
    val tokenStream = new CommonTokenStream(
      new token(CharStreams.fromString(script))
    )
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
  }
}
