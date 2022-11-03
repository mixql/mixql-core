package org.grenki.gsql.test

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.grenki.gsql.context.Context
import org.grenki.gsql.engine.Engine
import org.grenki.gsql.test.stub.StubEngine
import org.grenki.gsql.visitor.MainVisitor
import org.grenki.gsql.{sql, token}
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
