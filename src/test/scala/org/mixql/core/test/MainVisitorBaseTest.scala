package org.mixql.core.test

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.mixql.core.context.Context
import org.mixql.core.engine.Engine
import org.mixql.core.generated.{sql, token}
import org.mixql.core.test.stub.StubEngine
import org.mixql.core.visitor.MainVisitor
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.{Map => MutMap}

class MainVisitorBaseTest extends AnyFunSuite {
  def runMainVisitor(
    code: String,
    context: Context =
      new Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
  ): Context = {
    val lexer = new token(CharStreams.fromString(code))
    val tokenStream = new CommonTokenStream(
      new token(CharStreams.fromString(code))
    )
    tokenStream.getNumberOfOnChannelTokens // magic. if we do not do this tokenstream is empty
    val parser = new sql(new CommonTokenStream(lexer))
    new MainVisitor(context, tokenStream).visit(parser.program())
    context
  }
}
