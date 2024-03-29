package org.mixql

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.mixql.core.context.mtype._
import org.mixql.core.context.Context
import org.mixql.core.generated.{sql, token}
import org.mixql.core.visitor.MainVisitor

package object core {

  /** run script with context
    *
    * @param script
    *   to run
    * @param context
    *   run with this context
    */
  def run(script: String, context: Context): MType = {
    val lexer = new token(CharStreams.fromString(script))
    val tokenStream = new CommonTokenStream(new token(CharStreams.fromString(script)))
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
  }
}
