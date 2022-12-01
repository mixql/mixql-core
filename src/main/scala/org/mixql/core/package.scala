package org.mixql

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.mixql.core.context.gtype._
import org.mixql.core.context.Context
import org.mixql.core.visitor.MainVisitor
import org.mixql.core.parser.{token, sql}

package object core {

  /** run script on context
    *
    * @param script
    *   to run
    * @param context
    *   run on this context
    */
  def run(script: String, context: Context): Type = {
    val lexer = new token(CharStreams.fromString(script))
    val tokenStream = new CommonTokenStream(
      new token(CharStreams.fromString(script))
    )
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
  }
}
